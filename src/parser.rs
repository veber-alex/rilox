use crate::expr::Expr;
use crate::object::LoxObject;
use crate::report_error;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};
use std::borrow::Cow;
use std::fmt::Display;
use std::iter::Peekable;
use std::vec::IntoIter;

use TokenType::*;

#[derive(Debug)]
enum FunctionKind {
    Function,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function => f.write_str("function"),
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    in_loop: usize,
    in_fn: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
            in_loop: 0,
            in_fn: 0,
        }
    }

    pub fn parse(&mut self) -> Option<Vec<Stmt>> {
        let mut statements = vec![];

        while self.peek(|t| t != &Eof).is_some() {
            let stmt = self.declaration().map_err(|e| e.report()).ok()?;
            statements.push(stmt);
        }

        Some(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.next_if(|t| matches!(t, Var)).is_some() {
            self.var_declaration()
        } else if self.next_if(|t| matches!(t, Fun)).is_some() {
            self.function(FunctionKind::Function)
        } else {
            self.statement()
        }
        // FIXME: self.sync() HERE
    }

    fn function(&mut self, kind: FunctionKind) -> Result<Stmt, ParserError> {
        self.in_fn += 1;
        let name = self.verify(&Identifier, format!("Expect {} name", kind))?;
        self.verify(&LeftParen, format!("Expect '(' after {} name.", kind))?;

        let mut params = vec![];
        if self.peek(|t| t != &RightParen).is_some() {
            loop {
                if params.len() >= 255 {
                    if let Some(token) = self.peek(|_| true) {
                        // TODO: Non panicing error?
                        return Err(Self::error(token, "Can't have more than 255 parameters."));
                    }
                }
                let param = self.verify(&Identifier, "Expect parameter name.")?;
                params.push(param);
                if self.next_if(|t| t == &Comma).is_none() {
                    break;
                }
            }
        }
        self.verify(&RightParen, "Expect ')' after parameters.")?;

        self.verify(&LeftBrace, format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;
        self.in_fn -= 1;

        Ok(Stmt::function(name, params, body))
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.verify(&Identifier, "Expect variable name.")?;
        let initializer = self
            .next_if(|t| matches!(t, Equal))
            .map(|_| self.expression())
            .transpose()?;

        self.verify(&Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.next_if(|t| matches!(t, For)).is_some() {
            return self.for_statement();
        }

        if self.next_if(|t| matches!(t, If)).is_some() {
            return self.if_statement();
        }

        if self.next_if(|t| matches!(t, Print)).is_some() {
            return self.print_statement();
        }

        if let Some(token) = self.next_if(|t| matches!(t, Return)) {
            return self.return_statement(token);
        }

        if self.next_if(|t| matches!(t, While)).is_some() {
            return self.while_statement();
        }

        if let Some(token) = self.next_if(|t| matches!(t, Break)) {
            if self.in_loop > 0 {
                self.verify(&Semicolon, "Expect ';' after break.")?;
                return Ok(Stmt::break_stmt());
            } else {
                // FIXME: Non panic error
                return Err(Self::error(&token, "break not allowed outside of loops"));
            }
        }

        if self.next_if(|t| matches!(t, LeftBrace)).is_some() {
            return Ok(Stmt::block(self.block()?));
        }

        self.expression_statement()
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.verify(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::expr(value))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.verify(&LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.verify(&RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = self
            .next_if(|t| matches!(t, Else))
            .map(|_| self.statement())
            .transpose()?;

        Ok(Stmt::if_else(condition, then_branch, else_branch))
    }

    // FIXME: convert to match
    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.in_loop += 1;

        self.verify(&LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.next_if(|t| t == &Semicolon).is_some() {
            None
        } else if self.next_if(|t| t == &Var).is_some() {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = self
            .peek(|t| t != &Semicolon)
            .is_some()
            .then(|| self.expression())
            .transpose()?
            .unwrap_or_else(|| Expr::literal(LoxObject::bool(true)));
        self.verify(&Semicolon, "Expect ';' after loop condition.")?;

        let increment = self
            .peek(|t| t != &RightParen)
            .is_some()
            .then(|| self.expression())
            .transpose()?;
        self.verify(&RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;
        if let Some(increment) = increment {
            body = Stmt::block(vec![body, Stmt::expr(increment)])
        };
        body = Stmt::while_loop(condition, body);
        if let Some(initializer) = initializer {
            body = Stmt::block(vec![initializer, body])
        };

        self.in_loop -= 1;

        Ok(body)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.verify(&Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::print(value))
    }

    fn return_statement(&mut self, token: Token) -> Result<Stmt, ParserError> {
        if self.in_fn == 0 {
            return Err(Self::error(&token, "Can't return from top-level code."));
        }
        let value = self
            .peek(|t| t != &Semicolon)
            .is_some()
            .then(|| self.expression())
            .transpose()?;

        self.verify(&Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::return_stmt(token, value))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.in_loop += 1;

        self.verify(&LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.verify(&RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        self.in_loop -= 1;

        Ok(Stmt::while_loop(condition, body))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut statements = vec![];

        while !matches!(
            self.tokens.peek().map(|t| &t.ttype),
            Some(Eof) | Some(RightBrace)
        ) {
            statements.push(self.declaration()?)
        }
        self.verify(&RightBrace, "Expect '}' after block.")?;

        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        // parse l-value expression
        let expr = self.or()?;

        // Check for `=` token
        if let Some(token) = self.next_if(|t| matches!(t, Equal)) {
            // parse r-value expression
            let value = self.assignment()?;
            // verify l-value is a variable
            if let Expr::Variable(var) = expr {
                Ok(Expr::assign(var.name, value, var.id))
            } else {
                // TODO: Add flag to prevent SYNC and set it here
                Err(Self::error(&token, "Invalid assignment target."))
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;

        while let Some(operator) = self.next_if(|t| matches!(t, Or)) {
            let right = self.and()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;

        while let Some(operator) = self.next_if(|t| matches!(t, And)) {
            let right = self.equality()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self.next_if(|t| matches!(t, BangEqual | EqualEqual)) {
            let right = self.comparison()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let Some(operator) =
            self.next_if(|t| matches!(t, Greater | GreaterEqual | Less | LessEqual))
        {
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let Some(operator) = self.next_if(|t| matches!(t, Minus | Plus)) {
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(operator) = self.next_if(|t| matches!(t, Slash | Star)) {
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        let expr = if let Some(operator) = self.next_if(|t| matches!(t, Bang | Minus)) {
            Expr::unary(operator, self.unary()?)
        } else {
            self.call()?
        };

        Ok(expr)
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        while self.next_if(|t| t == &LeftParen).is_some() {
            expr = self.finish_call(expr)?;
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments = vec![];
        if self.peek(|t| t != &RightParen).is_some() {
            loop {
                if arguments.len() >= 255 {
                    if let Some(token) = self.peek(|_| true) {
                        // FIXME: Non panicing error
                        return Err(Self::error(token, "Can't have more than 255 arguments."));
                    }
                }
                arguments.push(self.expression()?);
                if self.next_if(|t| t == &Comma).is_none() {
                    break;
                }
            }
        }
        let paren = self.verify(&RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::call(callee, paren, arguments))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let token = self.tokens.next().expect("Tokens ended without Eof Token");

        let expr = match token.ttype {
            False => Expr::literal(LoxObject::bool(false)),
            True => Expr::literal(LoxObject::bool(true)),
            Nil => Expr::literal(LoxObject::nil()),
            Number(s) => Expr::literal(LoxObject::number(
                s.parse().expect("Incorrect Token for f64"),
            )),
            Str(s) => Expr::literal(LoxObject::string(s)),
            Identifier => Expr::variable(token),
            LeftParen => {
                let expr = self.expression()?;
                self.verify(&RightParen, "Expect ')' after expression.")?;
                Expr::grouping(expr)
            }
            _ => return Err(Self::error(&token, "Syntax Error")),
        };

        Ok(expr)
    }
}

// Helper functions
impl Parser {
    fn verify<T>(&mut self, ttype: &TokenType, message: T) -> Result<Token, ParserError>
    where
        T: Into<Cow<'static, str>>,
    {
        if let Some(token) = self.next_if(|t| t == ttype) {
            Ok(token)
        } else {
            let token = self.tokens.peek().expect("Tokens ended without Eof Token");
            Err(Self::error(token, message.into()))
        }
    }

    fn next_if<F>(&mut self, predicate: F) -> Option<Token>
    where
        F: FnOnce(&TokenType) -> bool,
    {
        self.tokens.next_if(|t| predicate(&t.ttype))
    }

    fn peek<F>(&mut self, predicate: F) -> Option<&Token>
    where
        F: FnOnce(&TokenType) -> bool,
    {
        self.tokens.peek().filter(|t| predicate(&t.ttype))
    }

    fn error<T>(token: &Token, message: T) -> ParserError
    where
        T: Into<Cow<'static, str>>,
    {
        if token.ttype == TokenType::Eof {
            ParserError::new(token.line, " at end".into(), message.into())
        } else {
            ParserError::new(
                token.line,
                format!(" at '{}'", token.lexeme).into(),
                message.into(),
            )
        }
    }
}

#[derive(Debug)]
struct ParserError {
    line: usize,
    loc: Cow<'static, str>,
    msg: Cow<'static, str>,
}

impl ParserError {
    pub fn new(line: usize, loc: Cow<'static, str>, msg: Cow<'static, str>) -> Self {
        Self { line, loc, msg }
    }

    pub fn report(self) -> Self {
        report_error("Parse", self.line, &self.loc, &self.msg);
        self
    }
}

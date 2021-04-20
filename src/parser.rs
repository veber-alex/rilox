use crate::expr::{Expr, VariableExpr};
use crate::model::object::LoxObject;
use crate::report_error;
use crate::stmt::{FunStmt, Stmt};
use crate::token::{Token, TokenKind};
use std::borrow::Cow;
use std::fmt::Display;
use std::iter::Peekable;
use std::vec::IntoIter;

use TokenKind::*;

#[derive(Debug)]
enum FunctionKind {
    Function,
    Method,
}

impl Display for FunctionKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            FunctionKind::Function => f.write_str("function"),
            FunctionKind::Method => f.write_str("method"),
        }
    }
}

#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
    //  TODO: Add lookback of Token for better error messages
    // lookback: Token,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Option<Vec<Stmt>> {
        let mut statements = vec![];

        while self.peek(|t| t != Eof).is_some() {
            let stmt = self.declaration().map_err(|e| e.report()).ok()?;
            statements.push(stmt);
        }

        Some(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.eat(Class) {
            return self.class_declaration();
        }

        if self.eat(Fun) {
            let fun_stmt = self.function(FunctionKind::Function)?;
            return Ok(Stmt::function(fun_stmt));
        }

        if self.eat(Var) {
            return self.var_declaration();
        }

        self.statement()

        // FIXME: self.sync() HERE
    }

    fn class_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.verify(Identifier, "Expect class name.")?;

        let superclass = if self.eat(Less) {
            let name = self.verify(Identifier, "Expect superclass name.")?;
            Some(VariableExpr::new(name))
        } else {
            None
        };

        self.verify(LeftBrace, "Expect '{' before class body.")?;

        let mut methods = vec![];
        while self.peek(|t| !matches!(t, RightBrace | Eof)).is_some() {
            methods.push(self.function(FunctionKind::Method)?)
        }
        self.verify(RightBrace, "Expect '}' after class body.")?;

        Ok(Stmt::class(name, methods, superclass))
    }

    fn function(&mut self, kind: FunctionKind) -> Result<FunStmt, ParserError> {
        let name = self.verify(Identifier, format!("Expect {} name", kind))?;
        self.verify(LeftParen, format!("Expect '(' after {} name.", kind))?;

        let mut params = vec![];
        if self.peek(|t| t != RightParen).is_some() {
            loop {
                if params.len() >= 255 {
                    if let Some(token) = self.peek(|_| true) {
                        // TODO: Non panicing error?
                        return Err(Self::error(token, "Can't have more than 255 parameters."));
                    }
                }
                let param = self.verify(Identifier, "Expect parameter name.")?;
                params.push(param);
                if !self.eat(Comma) {
                    break;
                }
            }
        }
        self.verify(RightParen, "Expect ')' after parameters.")?;

        self.verify(LeftBrace, format!("Expect '{{' before {} body.", kind))?;
        let body = self.block()?;

        Ok(Stmt::function_stmt(name, params, body))
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.verify(Identifier, "Expect variable name.")?;
        let initializer = self.eat(Equal).then(|| self.expression()).transpose()?;

        self.verify(Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.eat(For) {
            return self.for_statement();
        }

        if self.eat(If) {
            return self.if_statement();
        }

        if self.eat(Print) {
            return self.print_statement();
        }

        if let Some(token) = self.get(Return) {
            return self.return_statement(token);
        }

        if self.eat(While) {
            return self.while_statement();
        }

        if let Some(token) = self.get(Break) {
            self.verify(Semicolon, "Expect ';' after break.")?;
            return Ok(Stmt::break_stmt(token));
        }

        if self.eat(LeftBrace) {
            return Ok(Stmt::block(self.block()?));
        }

        self.expression_statement()
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.verify(Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::expr(value))
    }

    fn if_statement(&mut self) -> Result<Stmt, ParserError> {
        self.verify(LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.verify(RightParen, "Expect ')' after if condition.")?;

        let then_branch = self.statement()?;
        let else_branch = self.eat(Else).then(|| self.statement()).transpose()?;

        Ok(Stmt::if_else(condition, then_branch, else_branch))
    }

    fn for_statement(&mut self) -> Result<Stmt, ParserError> {
        self.verify(LeftParen, "Expect '(' after 'for'.")?;

        let initializer = if self.eat(Semicolon) {
            None
        } else if self.eat(Var) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = self
            .peek(|t| t != Semicolon)
            .is_some()
            .then(|| self.expression())
            .transpose()?
            .unwrap_or_else(|| Expr::literal(LoxObject::bool(true)));
        self.verify(Semicolon, "Expect ';' after loop condition.")?;

        let increment = self
            .peek(|t| t != RightParen)
            .is_some()
            .then(|| self.expression())
            .transpose()?;
        self.verify(RightParen, "Expect ')' after for clauses.")?;

        let mut body = self.statement()?;
        if let Some(increment) = increment {
            body = Stmt::block(vec![body, Stmt::expr(increment)])
        };
        body = Stmt::while_loop(condition, body);
        if let Some(initializer) = initializer {
            body = Stmt::block(vec![initializer, body])
        };

        Ok(body)
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.verify(Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::print(value))
    }

    fn return_statement(&mut self, token: Token) -> Result<Stmt, ParserError> {
        let value = self
            .peek(|t| t != Semicolon)
            .is_some()
            .then(|| self.expression())
            .transpose()?;

        self.verify(Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::return_stmt(token, value))
    }

    fn while_statement(&mut self) -> Result<Stmt, ParserError> {
        self.verify(LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.verify(RightParen, "Expect ')' after condition.")?;
        let body = self.statement()?;

        Ok(Stmt::while_loop(condition, body))
    }

    fn block(&mut self) -> Result<Vec<Stmt>, ParserError> {
        let mut statements = vec![];

        while self.peek(|t| !matches!(t, Eof | RightBrace)).is_some() {
            statements.push(self.declaration()?)
        }
        self.verify(RightBrace, "Expect '}' after block.")?;

        Ok(statements)
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, ParserError> {
        // parse l-value expression
        let expr = self.or()?;

        // Check for `=` token
        if let Some(token) = self.get(Equal) {
            // parse r-value expression
            let value = self.assignment()?;
            // verify l-value is a variable or get expression
            match expr {
                Expr::Variable(var) => Ok(Expr::assign(var.name, value, var.id)),
                Expr::Get(get) => Ok(Expr::set(get.object, get.name, value)),
                _ => Err(Self::error(&token, "Invalid assignment target.")),
            }
        } else {
            Ok(expr)
        }
    }

    fn or(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.and()?;

        while let Some(operator) = self.get(Or) {
            let right = self.and()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.equality()?;
        while let Some(operator) = self.get(And) {
            let right = self.equality()?;
            expr = Expr::logical(expr, operator, right);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self.get([BangEqual, EqualEqual]) {
            let right = self.comparison()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let Some(operator) = self.get([Greater, GreaterEqual, Less, LessEqual]) {
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let Some(operator) = self.get([Minus, Plus]) {
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(operator) = self.get([Slash, Star]) {
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        if let Some(operator) = self.get([Bang, Minus]) {
            Ok(Expr::unary(operator, self.unary()?))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.primary()?;
        loop {
            if self.eat(LeftParen) {
                expr = self.finish_call(expr)?;
            } else if self.eat(Dot) {
                let name = self.verify(Identifier, "Expect property name after '.'.")?;
                expr = Expr::get(name, expr);
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr, ParserError> {
        let mut arguments = vec![];
        if self.peek(|t| t != RightParen).is_some() {
            loop {
                if arguments.len() >= 255 {
                    if let Some(token) = self.peek(|_| true) {
                        // FIXME: Non panicing error
                        return Err(Self::error(token, "Can't have more than 255 arguments."));
                    }
                }
                arguments.push(self.expression()?);
                if !self.eat(Comma) {
                    break;
                }
            }
        }
        let paren = self.verify(RightParen, "Expect ')' after arguments.")?;

        Ok(Expr::call(callee, paren, arguments))
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let token = self.tokens.next().expect("Tokens ended without Eof Token");

        let expr = match token.kind {
            False => Expr::literal(LoxObject::bool(false)),
            True => Expr::literal(LoxObject::bool(true)),
            Nil => Expr::literal(LoxObject::nil()),
            Number => Expr::literal(LoxObject::number(
                token.lexeme.parse().expect("Incorrect Token for f64"),
            )),
            Str => Expr::literal(LoxObject::string(&token.lexeme[1..token.lexeme.len() - 1])),
            Identifier => Expr::variable(token),
            This => Expr::this(token),
            LeftParen => {
                let expr = self.expression()?;
                self.verify(RightParen, "Expect ')' after expression.")?;
                Expr::grouping(expr)
            }
            Super => {
                self.verify(Dot, "Expect '.' after 'super'.")?;
                let method = self.verify(Identifier, "Expect superclass method name.")?;
                Expr::super_expr(token, method)
            }
            FstringStart => {
                let mut parts = vec![];
                loop {
                    if self.eat(FstringEnd) {
                        break;
                    }

                    if let Some(token) = self.get(Str) {
                        parts.push(Expr::literal(LoxObject::string(&token.lexeme)))
                    }

                    if self.eat(LeftBrace) {
                        parts.push(self.expression()?);
                        self.verify(RightBrace, "Expected '}' after expression")?;
                    }
                }
                Expr::fstring(parts)
            }
            _ => return Err(Self::error(&token, "Syntax Error")),
        };

        Ok(expr)
    }
}

// Helper functions
impl Parser {
    fn verify<T>(&mut self, kind: TokenKind, message: T) -> Result<Token, ParserError>
    where
        T: Into<Cow<'static, str>>,
    {
        if let Some(token) = self.get(kind) {
            Ok(token)
        } else {
            let token = self.tokens.peek().expect("Tokens ended without Eof Token");
            Err(Self::error(token, message.into()))
        }
    }

    fn eat<T>(&mut self, kinds: T) -> bool
    where
        T: TokenKindMatches,
    {
        self.get(kinds).is_some()
    }

    fn get<T>(&mut self, kinds: T) -> Option<Token>
    where
        T: TokenKindMatches,
    {
        self.tokens.next_if(|t| kinds.matches(t.kind))
    }

    fn peek<F>(&mut self, predicate: F) -> Option<&Token>
    where
        F: FnOnce(TokenKind) -> bool,
    {
        self.tokens.peek().filter(|t| predicate(t.kind))
    }

    fn error<T>(token: &Token, message: T) -> ParserError
    where
        T: Into<Cow<'static, str>>,
    {
        if token.kind == TokenKind::Eof {
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

trait TokenKindMatches {
    fn matches(self, t: TokenKind) -> bool;
}

impl TokenKindMatches for TokenKind {
    fn matches(self, t: TokenKind) -> bool {
        self == t
    }
}

impl<const N: usize> TokenKindMatches for [TokenKind; N] {
    fn matches(self, t: TokenKind) -> bool {
        self.contains(&t)
    }
}

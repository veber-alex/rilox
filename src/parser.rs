use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;
use std::vec::IntoIter;

use crate::expr::Expr;
use crate::object::{LoxBool, LoxNil, LoxNumber, LoxString};
use crate::report_error;
use crate::stmt::Stmt;
use crate::token::{Token, TokenType};

use TokenType::*;

#[derive(Debug)]
pub struct Parser {
    tokens: Peekable<IntoIter<Token>>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens: tokens.into_iter().peekable(),
        }
    }

    pub fn parse(&mut self) -> Option<Vec<Stmt>> {
        let mut statements = vec![];

        while !matches!(self.tokens.peek().map(|t| &t.ttype), None | Some(Eof)) {
            let stmt = self.declaration().ok()?;
            statements.push(stmt);
        }

        Some(statements)
    }

    fn declaration(&mut self) -> Result<Stmt, ParserError> {
        if self.next_token_if(|t| matches!(t, Var)).is_some() {
            self.var_declaration()
        } else {
            self.statement()
        }
        // FIXME: SYNC HERE
    }

    fn var_declaration(&mut self) -> Result<Stmt, ParserError> {
        let name = self.next_token_verify(&Identifier, "Expect variable name.")?;
        let initializer = if self.next_token_if(|t| matches!(t, Equal)).is_some() {
            Some(self.expression()?)
        } else {
            None
        };
        self.next_token_verify(&Semicolon, "Expect ';' after variable declaration.")?;

        Ok(Stmt::var(name, initializer))
    }

    fn statement(&mut self) -> Result<Stmt, ParserError> {
        if self.next_token_if(|t| matches!(t, Print)).is_some() {
            self.print_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.next_token_verify(&Semicolon, "Expect ';' after value.")?;
        Ok(Stmt::print(value))
    }

    fn expression_statement(&mut self) -> Result<Stmt, ParserError> {
        let value = self.expression()?;
        self.next_token_verify(&Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::expr(value))
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self.next_token_if(|t| matches!(t, BangEqual | EqualEqual)) {
            let right = self.comparison()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let Some(operator) =
            self.next_token_if(|t| matches!(t, Greater | GreaterEqual | Less | LessEqual))
        {
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let Some(operator) = self.next_token_if(|t| matches!(t, Minus | Plus)) {
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(operator) = self.next_token_if(|t| matches!(t, Slash | Star)) {
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        let expr = if let Some(operator) = self.next_token_if(|t| matches!(t, Bang | Minus)) {
            Expr::unary(operator, self.unary()?)
        } else {
            self.primary()?
        };

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let token = self.tokens.next().expect("Tokens ended without Eof Token");

        let expr = match token.ttype {
            False => Expr::literal(LoxBool::new(false)),
            True => Expr::literal(LoxBool::new(true)),
            Nil => Expr::literal(LoxNil::new()),
            Number(s) => Expr::literal(LoxNumber::new(s.parse().expect("Incorrect Token for f64"))),
            Str(s) => Expr::literal(LoxString::new(s)),
            Identifier => Expr::variable(token),
            LeftParen => {
                let expr = self.expression()?;
                self.next_token_verify(&RightParen, "Expect ')' after expression.")?;
                Expr::grouping(expr)
            }
            _ => return Err(Self::error(&token, "Syntax Error")),
        };

        Ok(expr)
    }
}

// Helper functions
impl Parser {
    fn next_token_verify(
        &mut self,
        ttype: &TokenType,
        message: &str,
    ) -> Result<Token, ParserError> {
        if let Some(token) = self.next_token_if(|t| t == ttype) {
            Ok(token)
        } else {
            let token = self.tokens.peek().expect("Tokens ended without Eof Token");
            Err(Self::error(&token, message))
        }
    }

    fn next_token_if<F>(&mut self, predicate: F) -> Option<Token>
    where
        F: FnOnce(&TokenType) -> bool,
    {
        self.tokens.next_if(|t| predicate(&t.ttype))
    }

    fn error(token: &Token, message: &str) -> ParserError {
        if token.ttype == TokenType::Eof {
            report_error(token.line(), " at end", message)
        } else {
            report_error(token.line(), &format!(" at '{}'", token.lexeme()), message)
        }

        ParserError
    }
}

#[derive(Debug)]
struct ParserError;

impl Display for ParserError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("Parser Error")
    }
}

impl Error for ParserError {}

use std::error::Error;
use std::fmt::Display;
use std::iter::Peekable;
use std::mem;
use std::vec::IntoIter;

use crate::expr::Expr;
use crate::report_error;
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

    pub fn parse(&mut self) -> Option<Expr> {
        self.expression().ok()
    }

    fn expression(&mut self) -> Result<Expr, ParserError> {
        self.equality()
    }

    fn equality(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.comparison()?;
        while let Some(operator) = self
            .tokens
            .next_if(|t| matches!(t.ttype(), BangEqual | EqualEqual))
        {
            let right = self.comparison()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.term()?;
        while let Some(operator) = self
            .tokens
            .next_if(|t| matches!(t.ttype(), Greater | GreaterEqual | Less | LessEqual))
        {
            let right = self.term()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.factor()?;
        while let Some(operator) = self.tokens.next_if(|t| matches!(t.ttype(), Minus | Plus)) {
            let right = self.factor()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, ParserError> {
        let mut expr = self.unary()?;
        while let Some(operator) = self.tokens.next_if(|t| matches!(t.ttype(), Slash | Star)) {
            let right = self.unary()?;
            expr = Expr::binary(expr, operator, right)
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, ParserError> {
        let expr =
            if let Some(operator) = self.tokens.next_if(|t| matches!(t.ttype(), Bang | Minus)) {
                Expr::unary(operator, self.unary()?)
            } else {
                self.primary()?
            };

        Ok(expr)
    }

    fn primary(&mut self) -> Result<Expr, ParserError> {
        let mut token = self.tokens.next().expect("Tokens ended without Eof Token");

        let expr = match token.ttype_mut() {
            False => Expr::literal(false),
            True => Expr::literal(true),
            Nil => Expr::literal("nil"),
            Number(s) => Expr::literal(
                mem::take(s)
                    .parse::<f64>()
                    .expect("Incorrect Token for f64"),
            ),
            Str(s) => Expr::literal(mem::take(s)),
            LeftParen => {
                let expr = self.expression()?;
                self.verify_next_token(&RightParen, "Expect ')' after expression.")?;
                Expr::grouping(expr)
            }
            _ => return Err(Self::error(&token, "Syntax Error")),
        };

        Ok(expr)
    }
}

// Helper methods
impl Parser {
    fn verify_next_token(
        &mut self,
        ttype: &TokenType,
        message: &str,
    ) -> Result<Token, ParserError> {
        if let Some(token) = self.tokens.next_if(|t| t.ttype() == ttype) {
            return Ok(token);
        }

        match self.tokens.peek() {
            Some(token) => Err(Self::error(&token, message)),
            _ => unreachable!("Valid Token failed to parse"),
        }
    }

    fn error(token: &Token, message: &str) -> ParserError {
        if token.ttype() == &TokenType::Eof {
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

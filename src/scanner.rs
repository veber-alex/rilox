use crate::token::{Token, TokenType};
use crate::Rilox;
use std::fmt::Debug;

pub struct Scanner<'a> {
    rilox: &'a mut Rilox,
    source: &'a str,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str, rilox: &'a mut Rilox) -> Self {
        Self {
            rilox,
            source,
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<Token> {
        while !self.is_at_end() {
            self.start = self.current;
            self.scan_token();
        }

        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), None, self.line));

        self.tokens
    }

    fn scan_token(&mut self) {
        use TokenType::*;
        match self.advance() {
            '(' => self.add_token(LeftParen),
            ')' => self.add_token(RightParen),
            '{' => self.add_token(LeftBrace),
            '}' => self.add_token(RightBrace),
            ',' => self.add_token(Comma),
            '.' => self.add_token(Dot),
            '-' => self.add_token(Minus),
            '+' => self.add_token(Plus),
            ';' => self.add_token(Semicolon),
            '*' => self.add_token(Star),
            '!' => {
                let tt = if self.matches('=') { BangEqual } else { Bang };
                self.add_token(tt);
            }
            '=' => {
                let tt = if self.matches('=') { EqualEqual } else { Equal };
                self.add_token(tt);
            }
            '<' => {
                let tt = if self.matches('=') { LessEqual } else { Less };
                self.add_token(tt);
            }
            '>' => {
                let tt = if self.matches('=') {
                    GreaterEqual
                } else {
                    Greater
                };
                self.add_token(tt);
            }
            '/' => {
                if self.matches('/') {
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance();
                    }
                } else {
                    self.add_token(Slash)
                }
            }
            '"' => self.string(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.identifier(),
            ' ' | '\r' | '\t' => (),
            '\n' => self.line += 1,
            _ => self.rilox.error(self.line, "Unexpected character."),
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> char {
        // FIXME: Use Iterator properly
        let c = self.source.chars().nth(self.current).unwrap();
        self.current += 1;
        c
    }

    fn matches(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.source.chars().nth(self.current).unwrap() != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            '\0'
        } else {
            self.source.chars().nth(self.current).unwrap()
        }
    }

    fn peek_next(&mut self) -> char {
        if self.current + 1 >= self.source.len() {
            '\0'
        } else {
            self.source.chars().nth(self.current + 1).unwrap()
        }
    }

    fn is_digit(&self, c: char) -> bool {
        matches!(c, '0'..='9')
    }

    fn is_alpha(&self, c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_digit(c) || self.is_alpha(c)
    }

    fn string(&mut self) {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            self.rilox.error(self.line, "Unterminated string.");
            return;
        }

        // The closing '"'
        self.advance();

        // FIXME: Remove to_string.
        let value = self.source[self.start + 1..self.current - 1].to_string();
        self.add_token_with_literal(TokenType::String, value);
    }

    fn number(&mut self) {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // Look for a fractional part.
        let c = self.peek_next();
        if self.peek() == '.' && self.is_digit(c) {
            // Consume the "."
            self.advance();
        }

        while self.is_digit(self.peek()) {
            self.advance();
        }

        self.add_token_with_literal(
            TokenType::Number,
            self.source[self.start..self.current]
                .parse::<f64>()
                .unwrap(),
        )
    }

    fn keywords(&self, text: &str) -> TokenType {
        use TokenType::*;
        match text {
            "and" => And,
            "class" => Class,
            "else" => Else,
            "false" => False,
            "for" => For,
            "fun" => Fun,
            "if" => If,
            "nil" => Nil,
            "or" => Or,
            "print" => Print,
            "return" => Return,
            "super" => Super,
            "this" => This,
            "true" => True,
            "var" => Var,
            "while" => While,
            _ => Identifier,
        }
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }

        let text = &self.source[self.start..self.current];
        let tt = self.keywords(text);
        self.add_token(tt)
    }

    fn add_token(&mut self, ttype: TokenType) {
        // FIXME: Remove to_string
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token::new(ttype, text, None, self.line))
    }

    fn add_token_with_literal<T>(&mut self, ttype: TokenType, literal: T)
    where
        T: Debug + 'static,
    {
        // FIXME: Remove to_string
        let text = self.source[self.start..self.current].to_string();
        self.tokens
            .push(Token::new(ttype, text, Some(Box::new(literal)), self.line))
    }
}

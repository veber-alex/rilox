use crate::peek2::Peekable2;
use crate::token::{Token, TokenType};
use crate::{LoxType, Rilox};
use std::str::CharIndices;

pub struct Scanner<'a> {
    rilox: &'a mut Rilox,
    source: &'a str,
    source_iter: Peekable2<CharIndices<'a>>,
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
            source_iter: Peekable2::new(source.char_indices()),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
        }
    }

    pub fn scan_tokens(mut self) -> Vec<Token> {
        while self.source_iter.peek().is_some() {
            self.start = self.current;
            self.scan_token();
        }
        self.tokens
            .push(Token::new(TokenType::Eof, "".to_string(), None, self.line));

        self.tokens
    }

    fn scan_token(&mut self) {
        use TokenType::*;
        if let Some(c) = self.advance() {
            match c {
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
                    let tt = if self.advance_if_eq('=') {
                        BangEqual
                    } else {
                        Bang
                    };
                    self.add_token(tt);
                }
                '=' => {
                    let tt = if self.advance_if_eq('=') {
                        EqualEqual
                    } else {
                        Equal
                    };
                    self.add_token(tt);
                }
                '<' => {
                    let tt = if self.advance_if_eq('=') {
                        LessEqual
                    } else {
                        Less
                    };
                    self.add_token(tt);
                }
                '>' => {
                    let tt = if self.advance_if_eq('=') {
                        GreaterEqual
                    } else {
                        Greater
                    };
                    self.add_token(tt);
                }
                '/' => {
                    if self.advance_if_eq('/') {
                        self.advance_while(|c| c != '\n');
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
    }

    // FIXME: Running this method multiple times at the EOF will cause self.current
    // to go outside bounds because peek() keeps returning None
    fn update_current(&mut self) {
        if let Some(idx) = self.source_iter.peek().map(|&(idx, _)| idx) {
            self.current = idx
        } else {
            self.current += 1
        }
    }

    fn advance(&mut self) -> Option<char> {
        let (_, c) = self.source_iter.next()?;
        self.update_current();
        Some(c)
    }

    fn advance_if<F>(&mut self, predicate: F) -> bool
    where
        F: FnOnce(char) -> bool,
    {
        let to_advance = self.source_iter.next_if(|&(_, c)| predicate(c)).is_some();
        if to_advance {
            self.update_current();
        }
        to_advance
    }

    fn advance_if_eq(&mut self, expected: char) -> bool {
        self.advance_if(|c| c == expected)
    }

    // FIXME: Don't run update_current() in each loop, there is no need.
    fn advance_while<F>(&mut self, predicate: F)
    where
        F: Fn(char) -> bool,
    {
        while self.advance_if(&predicate) {}
    }

    fn peek(&mut self) -> Option<char> {
        self.source_iter.peek().map(|&(_, c)| c)
    }

    fn peek_next(&mut self) -> Option<char> {
        self.source_iter.peek_next().map(|&(_, c)| c)
    }

    fn is_digit(c: char) -> bool {
        matches!(c, '0'..='9')
    }

    fn is_alpha(c: char) -> bool {
        matches!(c, 'a'..='z' | 'A'..='Z' | '_')
    }

    fn is_alphanumeric(c: char) -> bool {
        Self::is_digit(c) || Self::is_alpha(c)
    }

    fn string(&mut self) {
        while matches!(self.peek(), Some(c) if c != '"') {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.peek().is_none() {
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
        self.advance_while(Self::is_digit);

        // Look for a fractional part.
        if matches!((self.peek(), self.peek_next()), (Some('.'), Some(c)) if Self::is_digit(c)) {
            // Consume the "."
            self.advance();

            // Consume the digits after the "."
            self.advance_while(Self::is_digit);
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
        self.advance_while(Self::is_alphanumeric);

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
        T: LoxType + 'static,
    {
        // FIXME: Remove to_string
        let text = self.source[self.start..self.current].to_string();
        self.tokens
            .push(Token::new(ttype, text, Some(Box::new(literal)), self.line))
    }
}

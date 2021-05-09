use crate::peek2::Peekable2;
use crate::report_error;
use crate::token::{Token, TokenKind};
use std::str::CharIndices;

macro_rules! number {
    () => {
        '0'..='9'
    };
}

macro_rules! alpha {
    () => {
        'a'..='z' | 'A'..='Z' | '_'
    };
}

pub struct Scanner<'a> {
    source: &'a str,
    source_iter: Peekable2<CharIndices<'a>>,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    had_error: bool,
    in_fstring_expr: bool,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a str) -> Self {
        Self {
            source,
            source_iter: Peekable2::new(source.char_indices()),
            tokens: vec![],
            start: 0,
            current: 0,
            line: 1,
            had_error: false,
            in_fstring_expr: false,
        }
    }

    pub fn scan_tokens(mut self) -> (Vec<Token>, bool) {
        while self.source_iter.peek().is_some() {
            self.scan_token()
        }
        self.tokens.push(Token::new(TokenKind::Eof, "", self.line));

        (self.tokens, self.had_error)
    }

    fn scan_token(&mut self) {
        use TokenKind::*;
        if let Some(c) = self.advance() {
            match c {
                '(' => self.add_token(LeftParen),
                ')' => self.add_token(RightParen),
                '{' => self.add_token(LeftBrace),
                '}' => {
                    self.add_token(RightBrace);
                    self.in_fstring_expr = false
                }
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
                '"' => {
                    if self.in_fstring_expr {
                        self.error(self.line, "Fstring expected '}'");
                        self.in_fstring_expr = false;
                    } else {
                        self.string()
                    }
                }
                'f' if self.advance_if_eq('"') => self.fstring(),
                number!() => self.number(),
                alpha!() => self.identifier(),
                ' ' | '\r' | '\t' => self.skip(),
                '\n' => {
                    self.line += 1;
                    self.skip()
                }
                _ => self.error(self.line, "Unexpected character."),
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

    fn string(&mut self) {
        while matches!(self.peek(), Some(c) if c != '"') {
            if self.peek() == Some('\n') {
                self.line += 1;
            }
            self.advance();
        }

        if self.peek().is_none() {
            self.error(self.line, "Unterminated string.");
            return;
        }

        // The closing '"'
        self.advance();

        self.add_token(TokenKind::Str);
    }

    fn fstring(&mut self) {
        self.add_token(TokenKind::FstringStart);

        while matches!(self.peek(), Some(c) if c != '"') {
            if self.peek() == Some('\n') {
                self.line += 1;
            }

            self.advance_while(|c| c != '{' && c != '"');
            if self.current > self.start {
                self.add_token(TokenKind::Str);
            }

            if self.peek() == Some('{') {
                // FIXME: better error handling + escaping
                self.in_fstring_expr = true;
                while self.in_fstring_expr {
                    self.scan_token();
                }
                if self.had_error {
                    return;
                }
            }
        }

        if self.peek().is_none() {
            self.error(self.line, "Unterminated fstring.");
            return;
        }

        // The closing '"'
        self.advance();

        self.add_token(TokenKind::FstringEnd);

        dbg!(&self.tokens);
    }

    fn number(&mut self) {
        let is_digit = |c| matches!(c, number!());

        self.advance_while(is_digit);

        // Look for a fractional part.
        if matches!((self.peek(), self.peek_next()), (Some('.'), Some(c)) if is_digit(c)) {
            // Consume the "."
            self.advance();

            // Consume the digits after the "."
            self.advance_while(is_digit);
        }

        self.add_token(TokenKind::Number)
    }

    fn keywords(&self, text: &str) -> TokenKind {
        use TokenKind::*;
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
            "break" => Break,
            _ => Identifier,
        }
    }

    fn identifier(&mut self) {
        self.advance_while(|c| matches!(c, number!() | alpha!()));

        let text = &self.source[self.start..self.current];
        let tt = self.keywords(text);
        self.add_token(tt)
    }

    fn add_token(&mut self, ttype: TokenKind) {
        let text = &self.source[self.start..self.current];
        self.tokens.push(Token::new(ttype, text, self.line));
        self.start = self.current;
    }

    fn skip(&mut self) {
        self.start = self.current;
    }

    fn error(&mut self, line: usize, message: &str) {
        self.had_error = true;
        report_error("Scan", line, "", message);
    }
}

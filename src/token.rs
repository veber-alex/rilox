use std::fmt::Debug;

#[rustfmt::skip]
#[derive(Debug, PartialEq)]
pub enum TokenType {
  // Single-character tokens.
  LeftParen, RightParen, LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

  // One or two character tokens.
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  Identifier, Str(String), Number(String),

  // Keywords.
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While,

  Eof
}

#[derive(Debug)]
pub struct Token {
    ttype: TokenType,
    lexeme: String,
    line: usize,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, line: usize) -> Self {
        Self {
            ttype,
            lexeme,
            line,
        }
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }

    pub fn ttype(&self) -> &TokenType {
        &self.ttype
    }

    pub fn ttype_mut(&mut self) -> &mut TokenType {
        &mut self.ttype
    }

    pub fn line(&self) -> usize {
        self.line
    }
}

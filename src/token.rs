use std::fmt::Debug;

use crate::LoxObject;

#[rustfmt::skip]
#[derive(Debug, Clone, Copy)]
// FIXME: Unleash the power of ADTs
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
  Identifier, String, Number,

  // Keywords.
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While,

  Eof
}

#[derive(Debug)]
pub struct Token {
    ttype: TokenType,
    lexeme: String,
    literal: Option<LoxObject>,
    line: usize,
}

impl Token {
    pub fn new(ttype: TokenType, lexeme: String, literal: Option<LoxObject>, line: usize) -> Self {
        Self {
            ttype,
            lexeme,
            literal,
            line,
        }
    }

    pub fn lexeme(&self) -> &str {
        &self.lexeme
    }
}

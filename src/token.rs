use std::fmt::Debug;

#[rustfmt::skip]
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum TokenKind {
  // Single-character tokens.
  LeftParen, RightParen, LeftBrace, RightBrace,
  Comma, Dot, Minus, Plus, Semicolon, Slash, Star,

  // One or two character tokens.
  Bang, BangEqual,
  Equal, EqualEqual,
  Greater, GreaterEqual,
  Less, LessEqual,

  // Literals.
  Identifier, Str, Number,

  // Keywords.
  And, Class, Else, False, Fun, For, If, Nil, Or,
  Print, Return, Super, This, True, Var, While, Break,

  // Markers
  FstringStart, FstringEnd,

  Eof
}

#[rustfmt::skip]
#[macro_export]
macro_rules! T {
    ('(') => {TokenKind::LeftParen};
    (')') => {TokenKind::RightParen};
    ('{') => {TokenKind::LeftBrace};
    ('}') => {TokenKind::RightBrace};
    (',') => {TokenKind::Comma};
    ('.') => {TokenKind::Dot};
    ('-') => {TokenKind::Minus};
    ('+') => {TokenKind::Plus};
    (';') => {TokenKind::Semicolon};
    ('/') => {TokenKind::Slash};
    ('*') => {TokenKind::Star};
}

#[derive(Debug, Clone, Copy)]
pub struct Token {
    pub kind: TokenKind,
    pub lexeme: &'static str,
    pub line: usize,
}

impl Token {
    pub fn new(kind: TokenKind, lexeme: &'static str, line: usize) -> Self {
        Self { kind, lexeme, line }
    }
}

use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum LoxObject {
    Number(LoxNumber),
    Nil,
    Bool(LoxBool),
    String(LoxString),
}

impl LoxObject {
    pub fn number(value: f64) -> LoxObject {
        Self::Number(LoxNumber(value))
    }

    pub fn nil() -> LoxObject {
        Self::Nil
    }

    pub fn bool(value: bool) -> LoxObject {
        Self::Bool(LoxBool(value))
    }

    pub fn string(value: String) -> LoxObject {
        Self::String(LoxString(Rc::from(value)))
    }

    pub fn type_as_str(&self) -> &'static str {
        match self {
            LoxObject::Number(_) => "Number",
            LoxObject::Nil => "Nil",
            LoxObject::Bool(_) => "Bool",
            LoxObject::String(_) => "String",
        }
    }
}

impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxObject::Number(v) => v.fmt(f),
            LoxObject::Nil => f.write_str("Nil"),
            LoxObject::Bool(v) => v.fmt(f),
            LoxObject::String(v) => v.fmt(f),
        }
    }
}

// Number
#[derive(Debug, PartialEq, Clone)]
pub struct LoxNumber(pub f64);

impl Display for LoxNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

// False + True
#[derive(Debug, PartialEq, Clone)]
pub struct LoxBool(pub bool);

impl Display for LoxBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

// String
#[derive(Debug, PartialEq, Clone)]
pub struct LoxString(pub Rc<str>);

impl Display for LoxString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

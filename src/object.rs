use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum LoxObject {
    Number(f64),
    Nil,
    Bool(bool),
    String(Rc<str>),
}

impl LoxObject {
    pub fn number(value: f64) -> LoxObject {
        Self::Number(value)
    }

    pub fn nil() -> LoxObject {
        Self::Nil
    }

    pub fn bool(value: bool) -> LoxObject {
        Self::Bool(value)
    }

    pub fn string(value: String) -> LoxObject {
        Self::String(Rc::from(value))
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

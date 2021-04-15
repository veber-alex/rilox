use super::instance::LoxInstance;
use crate::model::callable::LoxCallable;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum LoxObject {
    Number(f64),
    Nil,
    Bool(bool),
    String(Rc<str>),
    Instance(LoxInstance),
    Callable(LoxCallable),
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

    pub fn string(value: &str) -> LoxObject {
        Self::String(Rc::from(value))
    }

    pub fn callable(callable: LoxCallable) -> LoxObject {
        Self::Callable(callable)
    }

    pub fn instance(instance: LoxInstance) -> LoxObject {
        Self::Instance(instance)
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            Self::Nil => false,
            Self::Bool(b) => *b,
            _ => true,
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
            LoxObject::Callable(v) => v.fmt(f),
            LoxObject::Instance(v) => v.fmt(f),
        }
    }
}

use super::callable::BuiltinFn;
use super::class::LoxClass;
use super::function::LoxFunction;
use super::instance::LoxInstance;
use crate::environment::Environment;
use crate::model::callable::LoxCallable;
use crate::stmt::FunStmt;
use std::collections::HashMap;
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

    pub fn string(value: Rc<str>) -> LoxObject {
        Self::String(value)
    }

    pub fn instance(instance: LoxInstance) -> LoxObject {
        Self::Instance(instance)
    }

    pub fn function(declaration: FunStmt, closure: Environment, is_initializer: bool) -> LoxObject {
        Self::Callable(LoxCallable::Function(LoxFunction::new(
            declaration,
            closure,
            is_initializer,
        )))
    }

    pub fn class(
        name: Rc<str>,
        superclass: Option<LoxClass>,
        methods: HashMap<Rc<str>, LoxFunction>,
    ) -> LoxObject {
        LoxObject::Callable(LoxCallable::Class(LoxClass::new(name, superclass, methods)))
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
            LoxObject::Nil => f.write_str("nil"),
            LoxObject::Bool(v) => v.fmt(f),
            LoxObject::String(v) => v.fmt(f),
            LoxObject::Callable(v) => v.fmt(f),
            LoxObject::Instance(v) => v.fmt(f),
        }
    }
}

impl From<LoxFunction> for LoxObject {
    fn from(f: LoxFunction) -> Self {
        LoxObject::Callable(LoxCallable::Function(f))
    }
}

impl From<LoxClass> for LoxObject {
    fn from(c: LoxClass) -> Self {
        LoxObject::Callable(LoxCallable::Class(c))
    }
}

impl From<&'static dyn BuiltinFn> for LoxObject {
    fn from(obj: &'static dyn BuiltinFn) -> Self {
        LoxObject::Callable(LoxCallable::Builtin(obj))
    }
}

use super::callable::BuiltinFn;
use super::class::LoxClass;
use super::function::LoxFunction;
use super::instance::LoxInstance;
use crate::environment::Environment;
use crate::model::callable::LoxCallable;
use crate::stmt::FunStmt;

use rustc_hash::FxHashMap;

use std::fmt::Display;
use std::ops::Deref;
use std::rc::Rc;

#[derive(Debug)]
pub enum LoxObjectEnum {
    Number(f64),
    Nil,
    Bool(bool),
    StaticString(&'static str),
    String(String),
    Instance(LoxInstance),
    Callable(LoxCallable),
}

impl PartialEq for LoxObjectEnum {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Number(l0), Self::Number(r0)) => l0 == r0,
            (Self::Bool(l0), Self::Bool(r0)) => l0 == r0,
            (Self::StaticString(l0), Self::StaticString(r0)) => l0 == r0,
            (Self::String(l0), Self::String(r0)) => l0 == r0,
            (Self::String(l0), Self::StaticString(r0)) => &**l0 == *r0,
            (Self::StaticString(l0), Self::String(r0)) => *l0 == &**r0,
            (Self::Instance(l0), Self::Instance(r0)) => l0 == r0,
            (Self::Callable(l0), Self::Callable(r0)) => l0 == r0,
            _ => core::mem::discriminant(self) == core::mem::discriminant(other),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct LoxObject(pub Rc<LoxObjectEnum>);

impl Deref for LoxObject {
    type Target = LoxObjectEnum;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl Display for LoxObject {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.0.fmt(f)
    }
}

impl LoxObject {
    pub fn number(value: f64) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Number(value)))
    }

    pub fn nil() -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Nil))
    }

    pub fn bool(value: bool) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Bool(value)))
    }

    pub fn string(value: String) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::String(value)))
    }

    pub fn static_string(value: &'static str) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::StaticString(value)))
    }

    pub fn instance(instance: LoxInstance) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Instance(instance)))
    }

    pub fn callable(callable: LoxCallable) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Callable(callable)))
    }

    pub fn function(declaration: FunStmt, closure: Environment, is_initializer: bool) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Callable(LoxCallable::Function(
            LoxFunction::new(declaration, closure, is_initializer),
        ))))
    }

    pub fn class(
        name: &'static str,
        superclass: Option<LoxClass>,
        methods: FxHashMap<&'static str, LoxFunction>,
    ) -> LoxObject {
        LoxObject(Rc::new(LoxObjectEnum::Callable(LoxCallable::Class(
            LoxClass::new(name, superclass, methods),
        ))))
    }

    pub fn is_truthy(&self) -> bool {
        match &*self.0 {
            LoxObjectEnum::Nil => false,
            LoxObjectEnum::Bool(b) => *b,
            _ => true,
        }
    }
}

impl Display for LoxObjectEnum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxObjectEnum::Number(v) => v.fmt(f),
            LoxObjectEnum::Nil => f.write_str("nil"),
            LoxObjectEnum::Bool(v) => v.fmt(f),
            LoxObjectEnum::String(v) => v.fmt(f),
            LoxObjectEnum::Callable(v) => v.fmt(f),
            LoxObjectEnum::Instance(v) => v.fmt(f),
            LoxObjectEnum::StaticString(v) => v.fmt(f),
        }
    }
}

impl From<LoxFunction> for LoxObject {
    fn from(f: LoxFunction) -> Self {
        LoxObject::callable(LoxCallable::Function(f))
    }
}

impl From<LoxClass> for LoxObject {
    fn from(c: LoxClass) -> Self {
        LoxObject::callable(LoxCallable::Class(c))
    }
}

impl From<&'static dyn BuiltinFn> for LoxObject {
    fn from(obj: &'static dyn BuiltinFn) -> Self {
        LoxObject::callable(LoxCallable::Builtin(obj))
    }
}

use super::class::LoxClass;
use super::function::LoxFunction;
use super::object::LoxObject;
use crate::interpreter::{ControlFlow, Interpreter};
use std::fmt::Display;

pub trait BuiltinFn: Display + std::fmt::Debug {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, ControlFlow>;

    fn arity(&self) -> usize;

    fn name(&self) -> &'static str;
}

#[derive(Debug, Clone)]
pub enum LoxCallable {
    Builtin(&'static dyn BuiltinFn),
    Function(LoxFunction),
    Class(LoxClass),
}

impl LoxCallable {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, ControlFlow> {
        match self {
            LoxCallable::Builtin(v) => v.call(interpreter, arguments),
            LoxCallable::Function(v) => v.call(interpreter, arguments),
            LoxCallable::Class(v) => v.call(interpreter, arguments),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxCallable::Builtin(v) => v.arity(),
            LoxCallable::Function(v) => v.arity(),
            LoxCallable::Class(v) => v.arity(),
        }
    }
}

impl Display for LoxCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxCallable::Function(v) => v.fmt(f),
            LoxCallable::Builtin(v) => v.fmt(f),
            LoxCallable::Class(v) => v.fmt(f),
        }
    }
}

impl PartialEq for LoxCallable {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (LoxCallable::Builtin(b1), LoxCallable::Builtin(b2)) => b1.name() == b2.name(),
            (LoxCallable::Function(f1), LoxCallable::Function(f2)) => f1 == f2,
            (LoxCallable::Class(c1), LoxCallable::Class(c2)) => c1 == c2,
            _ => false,
        }
    }
}

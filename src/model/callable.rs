use super::class::LoxClass;
use super::function::LoxFunction;
use super::object::LoxObject;
use crate::enviroment::Enviroment;
use crate::interpreter::{ControlFlow, Interpreter};
use crate::stmt::FunStmt;
use std::collections::HashMap;
use std::fmt::Display;
use std::time::SystemTime;

#[derive(Debug, PartialEq, Clone)]
pub enum LoxCallable {
    Clock(Clock),
    Function(LoxFunction),
    Class(LoxClass),
}
impl LoxCallable {
    pub fn clock() -> LoxCallable {
        Self::Clock(Clock)
    }

    pub fn function(
        declaration: FunStmt,
        closure: Enviroment,
        is_initializer: bool,
    ) -> LoxCallable {
        Self::Function(LoxFunction::new(declaration, closure, is_initializer))
    }

    pub fn class(
        name: String,
        superclass: Option<LoxClass>,
        methods: HashMap<String, LoxFunction>,
    ) -> LoxCallable {
        Self::Class(LoxClass::new(name, superclass, methods))
    }

    pub fn class_from(class: LoxClass) -> LoxCallable {
        Self::Class(class)
    }

    pub fn function_from(func: LoxFunction) -> LoxCallable {
        Self::Function(func)
    }
}

impl LoxCallable {
    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, ControlFlow> {
        match self {
            LoxCallable::Clock(v) => v.call(interpreter, arguments),
            LoxCallable::Function(v) => v.call(interpreter, arguments),
            LoxCallable::Class(v) => v.call(interpreter, arguments),
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxCallable::Clock(v) => v.arity(),
            LoxCallable::Function(v) => v.arity(),
            LoxCallable::Class(v) => v.arity(),
        }
    }
}

impl Display for LoxCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxCallable::Function(v) => v.fmt(f),
            LoxCallable::Clock(v) => v.fmt(f),
            LoxCallable::Class(v) => v.fmt(f),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Clock;

impl Clock {
    pub fn call(&self, _: &mut Interpreter, _: Vec<LoxObject>) -> Result<LoxObject, ControlFlow> {
        let time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .expect("SystemTime before UNIX EPOCH!")
            .as_millis() as f64;

        Ok(LoxObject::number(time))
    }

    pub fn arity(&self) -> usize {
        0
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<native fn clock>")
    }
}

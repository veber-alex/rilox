use std::fmt::Display;
use std::time::SystemTime;

use crate::enviroment::Enviroment;
use crate::interpreter::{ControlFlow, Interpreter};
use crate::object::LoxObject;
use crate::stmt::FunStmt;

#[derive(Debug, PartialEq, Clone)]
pub enum LoxCallable {
    Clock(Clock),
    Function(LoxFunction),
}
impl LoxCallable {
    pub fn clock() -> LoxCallable {
        Self::Clock(Clock)
    }

    pub fn function(declaration: FunStmt, closure: Enviroment) -> LoxCallable {
        Self::Function(LoxFunction {
            declaration,
            closure,
        })
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
        }
    }

    pub fn arity(&self) -> usize {
        match self {
            LoxCallable::Clock(v) => v.arity(),
            LoxCallable::Function(v) => v.arity(),
        }
    }
}

impl Display for LoxCallable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            LoxCallable::Function(v) => v.fmt(f),
            LoxCallable::Clock(v) => v.fmt(f),
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

#[derive(Debug, PartialEq, Clone)]
pub struct LoxFunction {
    declaration: FunStmt,
    closure: Enviroment,
}

impl LoxFunction {
    fn call(
        &self,
        interpreter: &mut Interpreter,
        arguments: Vec<LoxObject>,
    ) -> Result<LoxObject, ControlFlow> {
        let env = Enviroment::with_enclosing(self.closure.clone());
        for (token, value) in self.declaration.params.iter().zip(arguments) {
            // FIXME: This is a String clone
            env.define(token.lexeme.clone(), value)
        }

        match interpreter.execute_block(&self.declaration.body, env) {
            Err(ControlFlow::Return(Some(v))) => Ok(v),
            Err(ControlFlow::Return(None)) => Ok(LoxObject::nil()),
            Ok(_) => Ok(LoxObject::nil()),
            Err(e) => Err(e),
        }
    }

    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<fn {}>", self.declaration.name.lexeme))
    }
}

use super::instance::LoxInstance;
use super::object::LoxObject;
use crate::enviroment::Enviroment;
use crate::expr::Location;
use crate::interpreter::{ControlFlow, Interpreter};
use crate::stmt::FunStmt;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct LoxFunction {
    pub is_initializer: bool,
    pub declaration: FunStmt,
    pub closure: Enviroment,
}

impl LoxFunction {
    pub fn new(declaration: FunStmt, closure: Enviroment, is_initializer: bool) -> Self {
        Self {
            is_initializer,
            declaration,
            closure,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter) -> Result<LoxObject, ControlFlow> {
        let env = Enviroment::with_enclosing(self.closure.clone());
        for value in interpreter.arguments_buffer.drain(..) {
            env.define(value);
        }

        match interpreter.execute_block(&self.declaration.body, env) {
            // init() always returns 'this'
            Ok(_) | Err(ControlFlow::Return(None)) if self.is_initializer => {
                Ok(self.closure.get_at(Location::new(0, 0)))
            }
            Err(ControlFlow::Return(Some(v))) => Ok(v),
            Err(ControlFlow::Return(None)) => Ok(LoxObject::nil()),
            Ok(_) => Ok(LoxObject::nil()),
            Err(e) => Err(e),
        }
    }

    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    pub fn bind(self, instance: LoxInstance) -> Self {
        let enviroment = Enviroment::with_enclosing(self.closure);
        enviroment.define(LoxObject::instance(instance));
        LoxFunction::new(self.declaration, enviroment, self.is_initializer)
    }
}

impl Display for LoxFunction {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<fn {}>", self.declaration.name.lexeme))
    }
}

impl PartialEq for LoxFunction {
    fn eq(&self, other: &Self) -> bool {
        self.declaration.name.lexeme == other.declaration.name.lexeme
    }
}

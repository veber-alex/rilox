use super::instance::LoxInstance;
use super::object::LoxObject;
use crate::environment::Environment;
use crate::expr::Location;
use crate::interpreter::{ACellOwner, ControlFlow, Interpreter};
use crate::stmt::FunStmt;
use std::fmt::Display;

#[derive(Debug, Clone)]
pub struct LoxFunction {
    pub is_initializer: bool,
    pub declaration: FunStmt,
    pub closure: Environment,
}

impl LoxFunction {
    pub fn new(declaration: FunStmt, closure: Environment, is_initializer: bool) -> Self {
        Self {
            is_initializer,
            declaration,
            closure,
        }
    }

    pub fn call(&self, interpreter: &mut Interpreter<'_>) -> Result<LoxObject, ControlFlow> {
        let env = Environment::with_enclosing(self.closure.clone());
        env.define_append(
            &mut interpreter.arguments_buffer,
            &mut interpreter.acell_owner,
        );

        match interpreter.execute_block(&self.declaration.body, env) {
            // init() always returns 'this'
            Ok(_) | Err(ControlFlow::Return(None)) if self.is_initializer => Ok(self
                .closure
                .get_at(Location::new(0, 0), &interpreter.acell_owner)),
            Err(ControlFlow::Return(Some(v))) => Ok(v),
            Ok(_) | Err(ControlFlow::Return(None)) => Ok(LoxObject::nil()),
            Err(e) => Err(e),
        }
    }

    pub fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    pub fn bind(self, instance: LoxInstance, owner: &mut ACellOwner) -> Self {
        let environment = Environment::with_enclosing(self.closure);
        environment.define(LoxObject::instance(instance), owner);
        LoxFunction::new(self.declaration, environment, self.is_initializer)
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

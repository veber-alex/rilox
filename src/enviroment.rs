use crate::interpreter::ControlFlow;
use crate::object::LoxObject;
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug, Default)]
struct Enviroment {
    values: HashMap<String, LoxObject>,
}

impl Enviroment {
    fn define(&mut self, name: String, value: LoxObject) {
        self.values.insert(name, value);
    }

    fn assign(&mut self, token: &Token, value: LoxObject) -> Result<(), LoxObject> {
        match self.values.get_mut(&token.lexeme) {
            Some(old) => {
                *old = value;
                Ok(())
            }
            None => Err(value),
        }
    }

    fn get(&mut self, token: &Token) -> Option<LoxObject> {
        self.values.get(&token.lexeme).cloned()
    }
}

#[derive(Debug)]
pub struct EnviromentStack {
    stack: Vec<Enviroment>,
}

impl Default for EnviromentStack {
    fn default() -> Self {
        Self {
            stack: vec![Enviroment::default()],
        }
    }
}

impl EnviromentStack {
    pub fn push(&mut self) {
        self.stack.push(Enviroment::default());
    }

    pub fn pop(&mut self) {
        self.stack.pop();
    }

    pub fn define(&mut self, name: String, value: LoxObject) {
        self.stack
            .last_mut()
            .expect("Empty env stack")
            .define(name, value);
    }

    pub fn assign(&mut self, token: &Token, mut value: LoxObject) -> Result<(), ControlFlow> {
        for env in self.stack.iter_mut().rev() {
            if let Err(v) = env.assign(token, value) {
                value = v;
            } else {
                return Ok(());
            }
        }

        Err(ControlFlow::new(
            token.line,
            format!("Undefined variable '{}'.", token.lexeme),
        ))
    }

    pub fn get(&mut self, token: &Token) -> Result<LoxObject, ControlFlow> {
        for env in self.stack.iter_mut().rev() {
            if let Some(token) = env.get(token) {
                return Ok(token);
            }
        }

        Err(ControlFlow::new(
            token.line,
            format!("Undefined variable '{}'.", token.lexeme),
        ))
    }
}

use crate::interpreter::RuntimeError;
use crate::object::LoxObject;
use crate::token::Token;
use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Enviroment {
    values: HashMap<String, LoxObject>,
}

impl Enviroment {
    pub fn define(&mut self, name: String, value: LoxObject) {
        self.values.insert(name, value);
    }

    pub fn get(&mut self, token: Token) -> Result<LoxObject, RuntimeError> {
        self.values.get(token.lexeme()).cloned().ok_or_else(|| {
            RuntimeError::new(
                token.line(),
                format!("Undefined variable '{}'.", token.lexeme()),
            )
        })
    }
}

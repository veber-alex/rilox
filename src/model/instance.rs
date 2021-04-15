use super::callable::LoxCallable;
use super::class::LoxClass;
use super::object::LoxObject;
use crate::interpreter::ControlFlow;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, Clone)]
pub struct LoxInstance {
    pub class: LoxClass,
    // TODO: Replace this Rc with Gc to collect cycles
    pub fields: Rc<RefCell<HashMap<String, LoxObject>>>,
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: Default::default(),
        }
    }

    pub fn get(&self, token: &Token) -> Result<LoxObject, ControlFlow> {
        // property from instance
        if let Some(obj) = self.fields.borrow().get(&token.lexeme).cloned() {
            return Ok(obj);
        }

        // method from class - bind to instance
        if let Some(obj) = self.class.find_method(&token.lexeme) {
            let function = obj.bind(self.clone());
            return Ok(LoxObject::Callable(LoxCallable::Function(function)));
        }

        Err(ControlFlow::abort(
            token.line,
            format!("Undefined property '{}'.", token.lexeme),
        ))
    }

    pub fn set(&self, token: &Token, value: LoxObject) {
        self.fields.borrow_mut().insert(token.lexeme.clone(), value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<instance of '{}'>", self.class.0.name))
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(&*self.fields, &*other.fields)
    }
}

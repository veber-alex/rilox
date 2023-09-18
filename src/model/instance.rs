use super::callable::LoxCallable;
use super::class::LoxClass;
use super::object::LoxObject;
use crate::interpreter::{ACell, ACellOwner, ControlFlow};
use crate::token::Token;

use rustc_hash::FxHashMap;

use std::fmt::Display;
use std::rc::Rc;

#[derive(Clone)]
pub struct LoxInstance {
    pub class: LoxClass,
    // TODO: Replace this Rc with Gc to collect cycles
    pub fields: Rc<ACell<FxHashMap<&'static str, LoxObject>>>,
}

impl std::fmt::Debug for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("LoxInstance")
            .field("class", &self.class)
            .finish()
    }
}

impl LoxInstance {
    pub fn new(class: LoxClass) -> Self {
        Self {
            class,
            fields: Rc::new(ACell::new(FxHashMap::default())),
        }
    }

    pub fn get(&self, token: &Token, owner: &mut ACellOwner) -> Result<LoxObject, ControlFlow> {
        // property from instance
        if let Some(obj) = owner.ro(&self.fields).get(token.lexeme).cloned() {
            return Ok(obj);
        }

        // method from class - bind to instance
        if let Some(obj) = self.class.find_method(token.lexeme) {
            let function = obj.bind(self.clone(), owner);
            return Ok(LoxObject::callable(LoxCallable::Function(function)));
        }

        Err(ControlFlow::abort(
            token.line,
            format!("Undefined property '{}'.", token.lexeme),
        ))
    }

    pub fn set(&self, token: &Token, value: LoxObject, owner: &mut ACellOwner) {
        owner.rw(&self.fields).insert(token.lexeme, value);
    }
}

impl Display for LoxInstance {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<instance of '{}'>", self.class.0.name))
    }
}

impl PartialEq for LoxInstance {
    fn eq(&self, other: &Self) -> bool {
        // compare the Rc pointer address, otherwise 2 instances from the same class
        // with the same fields will compare as equal.
        std::ptr::eq(&*self.fields, &*other.fields)
    }
}

impl Eq for LoxInstance {}

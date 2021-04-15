use crate::interpreter::ControlFlow;
use crate::model::object::LoxObject;
use crate::token::Token;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, Default, PartialEq)]
pub struct EnviromentInner {
    enclosing: Option<Enviroment>,
    values: RefCell<HashMap<String, LoxObject>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
// TODO: Try to remove this Rc
pub struct Enviroment(pub Rc<EnviromentInner>);

impl Enviroment {
    pub fn with_enclosing(enclosing: Enviroment) -> Self {
        Self(Rc::new(EnviromentInner {
            enclosing: Some(enclosing),
            values: Default::default(),
        }))
    }

    pub fn define(&self, name: String, value: LoxObject) {
        self.0.values.borrow_mut().insert(name, value);
    }

    pub fn assign(&self, token: &Token, value: LoxObject) -> Result<(), ControlFlow> {
        // Try the local env
        if let Some(obj) = self.0.values.borrow_mut().get_mut(&token.lexeme) {
            *obj = value;
            return Ok(());
        }

        // // Try the enclosing env
        // if let Some(env) = &self.0.enclosing {
        //     return env.assign(token, value);
        // }

        // Var not found, error
        Err(ControlFlow::abort(
            token.line,
            format!("Undefined variable '{}'.", token.lexeme),
        ))
    }

    pub fn get(&self, token: &Token) -> Result<LoxObject, ControlFlow> {
        // Try the local env
        if let Some(obj) = self.0.values.borrow().get(&token.lexeme) {
            return Ok(obj.clone());
        }

        // // Try the enclosing env
        // if let Some(env) = &self.0.enclosing {
        //     return env.get(token);
        // }

        // Var not found, error
        Err(ControlFlow::abort(
            token.line,
            format!("Undefined variable '{}'.", token.lexeme),
        ))
    }

    fn ancestor(&self, distance: usize) -> &Enviroment {
        let mut enviroment = Some(self);
        for _ in 0..distance {
            enviroment = enviroment.and_then(|e| e.0.enclosing.as_ref())
        }

        enviroment.expect("Enviroment at distance not found")
    }

    pub fn get_at(&self, distance: usize, name: &str) -> LoxObject {
        self.ancestor(distance)
            .0
            .values
            .borrow()
            .get(name)
            .unwrap_or_else(|| {
                panic!(
                    "Resolved variable not found. distance: {:?},  name: {:?}",
                    distance, name
                )
            })
            .clone()
    }

    pub fn assign_at(
        &self,
        distance: usize,
        token: &Token,
        value: LoxObject,
    ) -> Result<(), ControlFlow> {
        *self
            .ancestor(distance)
            .0
            .values
            .borrow_mut()
            .get_mut(&token.lexeme)
            .expect("Resolved variable not found") = value;

        Ok(())
    }
}

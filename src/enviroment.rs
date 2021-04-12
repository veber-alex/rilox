use crate::interpreter::ControlFlow;
use crate::object::LoxObject;
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
pub struct Enviroment(Rc<EnviromentInner>);

impl Enviroment {
    pub fn with_enclosing(enclosing: Enviroment) -> Self {
        Self(Rc::new(EnviromentInner {
            enclosing: Some(enclosing),
            values: RefCell::new(HashMap::new()),
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
        let mut enviroment = self;
        for _ in 0..distance {
            enviroment = &enviroment
                .0
                .enclosing
                .as_ref()
                .expect("Enviroment at distance not found");
        }

        enviroment
    }

    pub fn get_at(&self, distance: usize, name: &Token) -> Result<LoxObject, ControlFlow> {
        Ok(self
            .ancestor(distance)
            .0
            .values
            .borrow()
            .get(&name.lexeme)
            .expect("Resolved variable not found")
            .clone())
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

use super::function::LoxFunction;
use super::instance::LoxInstance;
use super::object::LoxObject;
use crate::interpreter::{ControlFlow, Interpreter};
use std::collections::HashMap;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug)]
pub struct LoxClassInner {
    pub name: Rc<str>,
    pub superclass: Option<LoxClass>,
    pub methods: HashMap<Rc<str>, LoxFunction>,
}

#[derive(Debug, Clone)]
pub struct LoxClass(pub Rc<LoxClassInner>);

impl LoxClass {
    pub fn new(
        name: Rc<str>,
        superclass: Option<Self>,
        methods: HashMap<Rc<str>, LoxFunction>,
    ) -> Self {
        let inner = LoxClassInner {
            name,
            superclass,
            methods,
        };
        Self(Rc::new(inner))
    }

    pub fn find_method(&self, name: &str) -> Option<LoxFunction> {
        self.0
            .methods
            .get(name)
            .cloned()
            .or_else(|| self.0.superclass.as_ref().and_then(|c| c.find_method(name)))
    }

    pub fn call(&self, interpreter: &mut Interpreter) -> Result<LoxObject, ControlFlow> {
        let instance = LoxInstance::new(self.clone());
        let initializer = self.find_method("init");

        if let Some(init) = initializer {
            init.bind(instance.clone()).call(interpreter)?;
        }

        Ok(LoxObject::instance(instance))
    }

    pub fn arity(&self) -> usize {
        self.find_method("init").map(|m| m.arity()).unwrap_or(0)
    }
}

impl Display for LoxClass {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_fmt(format_args!("<class '{}'>", self.0.name))
    }
}

impl PartialEq for LoxClass {
    fn eq(&self, other: &Self) -> bool {
        self.0.name == other.0.name
    }
}

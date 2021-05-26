use crate::expr::Location;
use crate::model::object::LoxObject;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Default, PartialEq)]
pub struct EnvironmentInner {
    enclosing: Option<Environment>,
    values: RefCell<Vec<LoxObject>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Environment(Rc<EnvironmentInner>);

impl Environment {
    pub fn with_enclosing(enclosing: Environment) -> Self {
        Self(Rc::new(EnvironmentInner {
            enclosing: Some(enclosing),
            values: Default::default(),
        }))
    }

    /// Defines a new variable and returns it's index in the environment
    pub fn define(&self, value: LoxObject) -> usize {
        let mut values = self.0.values.borrow_mut();
        values.push(value);
        values.len() - 1
    }

    pub fn define_append(&self, source: &mut Vec<LoxObject>) {
        self.0.values.borrow_mut().append(source);
    }

    fn ancestor(&self, distance: usize) -> &Environment {
        let mut environment = Some(self);
        for _ in 0..distance {
            environment = environment.and_then(|e| e.0.enclosing.as_ref())
        }

        environment.expect("Environment at distance not found")
    }

    pub fn get_at(&self, loc: Location) -> LoxObject {
        self.ancestor(loc.distance)
            .0
            .values
            .borrow()
            .get(loc.index)
            .unwrap_or_else(|| {
                panic!(
                    "Resolved variable not found. distance: {:?},  index: {:?}",
                    loc.distance, loc.index
                )
            })
            .clone()
    }

    pub fn assign_at(&self, loc: Location, value: LoxObject) {
        *self
            .ancestor(loc.distance)
            .0
            .values
            .borrow_mut()
            .get_mut(loc.index)
            .unwrap_or_else(|| {
                panic!(
                    "Resolved variable not found. distance: {:?},  index: {:?}",
                    loc.distance, loc.index
                )
            }) = value;
    }
}

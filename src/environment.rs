use crate::expr::Location;
use crate::interpreter::{ACell, ACellOwner};
use crate::model::object::LoxObject;
use std::rc::Rc;

pub struct EnvironmentInner {
    enclosing: Option<Environment>,
    values: ACell<Vec<LoxObject>>,
}

impl Default for EnvironmentInner {
    fn default() -> Self {
        Self {
            enclosing: Default::default(),
            values: ACell::new(vec![]),
        }
    }
}

impl std::fmt::Debug for EnvironmentInner {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("EnvironmentInner")
            .field("enclosing", &self.enclosing)
            .finish()
    }
}

#[derive(Debug, Default, Clone)]
pub struct Environment(Rc<EnvironmentInner>);

impl Environment {
    pub fn with_enclosing(enclosing: Environment) -> Self {
        Self(Rc::new(EnvironmentInner {
            enclosing: Some(enclosing),
            values: ACell::new(vec![]),
        }))
    }

    /// Defines a new variable and returns it's index in the environment
    pub fn define(&self, value: LoxObject, owner: &mut ACellOwner) -> usize {
        let values = owner.rw(&self.0.values);
        values.push(value);
        values.len() - 1
    }

    pub fn define_append(&self, source: &mut Vec<LoxObject>, owner: &mut ACellOwner) {
        owner.rw(&self.0.values).append(source);
    }

    fn ancestor(&self, distance: usize) -> &Environment {
        let mut environment = Some(self);
        for _ in 0..distance {
            environment = environment.and_then(|e| e.0.enclosing.as_ref())
        }

        environment.expect("Environment at distance not found")
    }

    pub fn get_at(&self, loc: Location, owner: &ACellOwner) -> LoxObject {
        owner
            .ro(&self.ancestor(loc.distance).0.values)
            .get(loc.index)
            .unwrap_or_else(|| {
                panic!(
                    "Resolved variable not found. distance: {:?},  index: {:?}",
                    loc.distance, loc.index
                )
            })
            .clone()
    }

    pub fn assign_at(&self, loc: Location, value: LoxObject, owner: &mut ACellOwner) {
        *owner
            .rw(&self.ancestor(loc.distance).0.values)
            .get_mut(loc.index)
            .unwrap_or_else(|| {
                panic!(
                    "Resolved variable not found. distance: {:?},  index: {:?}",
                    loc.distance, loc.index
                )
            }) = value;
    }
}

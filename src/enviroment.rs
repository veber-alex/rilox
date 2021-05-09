use crate::expr::Location;
use crate::model::object::LoxObject;
use std::cell::RefCell;
use std::rc::Rc;

#[derive(Debug, Default, PartialEq)]
pub struct EnviromentInner {
    enclosing: Option<Enviroment>,
    values: RefCell<Vec<LoxObject>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Enviroment(Rc<EnviromentInner>);

impl Enviroment {
    pub fn with_enclosing(enclosing: Enviroment) -> Self {
        Self(Rc::new(EnviromentInner {
            enclosing: Some(enclosing),
            values: Default::default(),
        }))
    }

    /// Defines a new variable and returns it's index in the enviroment
    pub fn define(&self, value: LoxObject) -> usize {
        let mut values = self.0.values.borrow_mut();
        values.push(value);
        values.len() - 1
    }

    pub fn define_many<I>(&self, iter: I)
    where
        I: Iterator<Item = LoxObject>,
    {
        self.0.values.borrow_mut().extend(iter);
    }

    fn ancestor(&self, distance: usize) -> &Enviroment {
        let mut enviroment = Some(self);
        for _ in 0..distance {
            enviroment = enviroment.and_then(|e| e.0.enclosing.as_ref())
        }

        enviroment.expect("Enviroment at distance not found")
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

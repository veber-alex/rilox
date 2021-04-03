#![allow(clippy::new_ret_no_self)]

use std::any::Any;
// FIXME: Use a faster hasher
use std::collections::hash_map::DefaultHasher;
use std::fmt::{Debug, Display};
use std::hash::{Hash, Hasher};
use std::rc::Rc;

pub trait LoxType: Debug + Display + Any {
    fn as_any(&self) -> &dyn Any;
    fn hash(&self) -> u64;
    fn type_as_str(&self) -> &'static str;
}

pub type LoxObject = Rc<dyn LoxType>;

// Number
#[derive(Debug)]
pub struct LoxNumber(pub f64);

impl LoxNumber {
    pub fn new(value: f64) -> LoxObject {
        Rc::new(Self(value))
    }
}

impl Display for LoxNumber {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl LoxType for LoxNumber {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.0.to_ne_bytes().hash(&mut hasher);
        hasher.finish()
    }

    fn type_as_str(&self) -> &'static str {
        "Number"
    }
}

// Nil
#[derive(Debug)]
pub struct LoxNil;

impl LoxNil {
    pub fn new() -> LoxObject {
        Rc::new(LoxNil)
    }
}

impl Display for LoxNil {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt("Nil", f)
    }
}

impl LoxType for LoxNil {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        ().hash(&mut hasher);
        hasher.finish()
    }

    fn type_as_str(&self) -> &'static str {
        "Nil"
    }
}

// False + True
#[derive(Debug)]
pub struct LoxBool(pub bool);

impl LoxBool {
    pub fn new(value: bool) -> LoxObject {
        Rc::new(LoxBool(value))
    }
}

impl Display for LoxBool {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl LoxType for LoxBool {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish()
    }

    fn type_as_str(&self) -> &'static str {
        "Bool"
    }
}

// String
#[derive(Debug)]
// FIXME: Box<str> ?
pub struct LoxString(pub String);

impl LoxString {
    pub fn new(value: String) -> LoxObject {
        Rc::new(LoxString(value))
    }
}

impl Display for LoxString {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        Display::fmt(&self.0, f)
    }
}

impl LoxType for LoxString {
    fn as_any(&self) -> &dyn Any {
        self
    }

    fn hash(&self) -> u64 {
        let mut hasher = DefaultHasher::new();
        self.0.hash(&mut hasher);
        hasher.finish()
    }

    fn type_as_str(&self) -> &'static str {
        "String"
    }
}

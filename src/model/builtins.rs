use super::callable::BuiltinFn;
use crate::interpreter::{ControlFlow, Interpreter};
use crate::model::object::LoxObject;
use crate::resolver::Resolver;
use std::fmt::Display;
use std::time::SystemTime;

#[derive(Debug, PartialEq, Clone)]
pub struct Clock;

impl BuiltinFn for Clock {
    fn call(&self, _: &mut Interpreter<'_>) -> Result<LoxObject, ControlFlow> {
        let time = SystemTime::now()
            .duration_since(SystemTime::UNIX_EPOCH)
            .unwrap_or_default()
            .as_millis() as f64;

        Ok(LoxObject::number(time))
    }

    fn arity(&self) -> usize {
        0
    }

    fn name(&self) -> &'static str {
        "clock"
    }
}

impl Display for Clock {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.write_str("<native fn clock>")
    }
}

pub fn install_builtins(interpreter: &mut Interpreter<'_>, resolver: &mut Resolver<'_>) {
    let clock: &'static dyn BuiltinFn = &Clock;

    let builtins = [clock];
    for obj in builtins {
        resolver.scopes[0].insert(obj.name(), true);
        interpreter
            .environment
            .define(obj.into(), &mut interpreter.acell_owner);
    }
}

use crate::arena::Arena;
use crate::expr::Expr;

#[derive(Debug, Default)]
pub struct Context {
    pub expr: Arena<Expr>,
}

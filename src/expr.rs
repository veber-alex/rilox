use crate::model::object::LoxObject;
use crate::token::Token;
use std::cell::Cell;
use std::fmt::Debug;

#[derive(Debug, Default)]
struct VarIdCounter(Cell<usize>);

impl VarIdCounter {
    fn next(&self) -> usize {
        self.0.replace(self.0.get() + 1)
    }
}

thread_local! {
    static VARID: VarIdCounter = VarIdCounter::default();
}

pub trait ExprVisitor {
    type Output;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output;
    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Self::Output;
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::Output;
    fn visit_get_expr(&mut self, expr: &GetExpr) -> Self::Output;
    fn visit_set_expr(&mut self, expr: &SetExpr) -> Self::Output;
    fn visit_this_expr(&mut self, expr: &ThisExpr) -> Self::Output;
}

#[derive(Debug)]
pub struct UnboxedBinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct UnboxedGroupingExpr {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub value: LoxObject,
}

#[derive(Debug)]
pub struct UnboxedUnrayExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Token,
    pub id: usize,
}

#[derive(Debug)]
pub struct UnboxedAssignExpr {
    pub name: Token,
    pub value: Expr,
    pub id: usize,
}

#[derive(Debug)]
pub struct UnboxedCallExpr {
    pub callee: Expr,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct UnboxedLogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct UnboxedGetExpr {
    pub name: Token,
    pub object: Expr,
}

#[derive(Debug)]
pub struct UnboxedSetExpr {
    pub object: Expr,
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug)]
pub struct ThisExpr {
    pub keyword: Token,
    pub id: usize,
}

pub type BinaryExpr = Box<UnboxedBinaryExpr>;
pub type GroupingExpr = Box<UnboxedGroupingExpr>;
pub type UnaryExpr = Box<UnboxedUnrayExpr>;
pub type AssignExpr = Box<UnboxedAssignExpr>;
pub type LogicalExpr = Box<UnboxedLogicalExpr>;
pub type CallExpr = Box<UnboxedCallExpr>;
pub type GetExpr = Box<UnboxedGetExpr>;
pub type SetExpr = Box<UnboxedSetExpr>;

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
    Assign(AssignExpr),
    Logical(LogicalExpr),
    Call(CallExpr),
    Get(GetExpr),
    Set(SetExpr),
    This(ThisExpr),
}

impl Expr {
    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Output {
        match self {
            Expr::Binary(e) => visitor.visit_binary_expr(e),
            Expr::Grouping(e) => visitor.visit_grouping_expr(e),
            Expr::Literal(e) => visitor.visit_literal_expr(e),
            Expr::Unary(e) => visitor.visit_unary_expr(e),
            Expr::Variable(e) => visitor.visit_variable_expr(e),
            Expr::Assign(e) => visitor.visit_assign_expr(e),
            Expr::Logical(e) => visitor.visit_logical_expr(e),
            Expr::Call(e) => visitor.visit_call_expr(e),
            Expr::Get(e) => visitor.visit_get_expr(e),
            Expr::Set(e) => visitor.visit_set_expr(e),
            Expr::This(e) => visitor.visit_this_expr(e),
        }
    }

    pub fn binary(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Binary(Box::new(UnboxedBinaryExpr {
            left,
            operator,
            right,
        }))
    }

    pub fn grouping(expression: Expr) -> Expr {
        Expr::Grouping(Box::new(UnboxedGroupingExpr { expression }))
    }

    pub fn literal(value: LoxObject) -> Expr {
        Expr::Literal(LiteralExpr { value })
    }

    pub fn unary(operator: Token, right: Expr) -> Expr {
        Expr::Unary(Box::new(UnboxedUnrayExpr { operator, right }))
    }

    pub fn variable(name: Token) -> Expr {
        Expr::Variable(VariableExpr {
            name,
            id: VARID.with(|v| v.next()),
        })
    }

    pub fn assign(name: Token, value: Expr, id: usize) -> Expr {
        Expr::Assign(Box::new(UnboxedAssignExpr { name, value, id }))
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Logical(Box::new(UnboxedLogicalExpr {
            left,
            operator,
            right,
        }))
    }

    pub fn call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Expr {
        Expr::Call(Box::new(UnboxedCallExpr {
            callee,
            paren,
            arguments,
        }))
    }

    pub fn get(name: Token, object: Expr) -> Expr {
        Expr::Get(Box::new(UnboxedGetExpr { name, object }))
    }

    pub fn set(object: Expr, name: Token, value: Expr) -> Expr {
        Expr::Set(Box::new(UnboxedSetExpr {
            object,
            name,
            value,
        }))
    }

    pub fn this(keyword: Token) -> Expr {
        Expr::This(ThisExpr {
            keyword,
            id: VARID.with(|v| v.next()),
        })
    }
}

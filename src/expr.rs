use crate::model::object::LoxObject;
use crate::token::Token;
use std::cell::Cell;
use std::fmt::Debug;

fn uid() -> usize {
    thread_local! {
        static VARID: Cell<usize> = Cell::new(0);
    }
    VARID.with(|v| v.replace(v.get() + 1))
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
    fn visit_super_expr(&mut self, expr: &SuperExpr) -> Self::Output;
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct GroupingExpr {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub value: LoxObject,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Token,
    pub id: usize,
}

impl VariableExpr {
    pub fn new(name: Token) -> Self {
        Self { name, id: uid() }
    }
}

#[derive(Debug)]
pub struct AssignExpr {
    pub name: Token,
    pub value: Expr,
    pub id: usize,
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Expr,
    pub paren: Token,
    pub arguments: Vec<Expr>,
}

#[derive(Debug)]
pub struct LogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

#[derive(Debug)]
pub struct GetExpr {
    pub name: Token,
    pub object: Expr,
}

#[derive(Debug)]
pub struct SetExpr {
    pub object: Expr,
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug)]
pub struct ThisExpr {
    pub keyword: Token,
    pub id: usize,
}

#[derive(Debug)]
pub struct SuperExpr {
    pub keyword: Token,
    pub method: Token,
    pub id: usize,
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(LiteralExpr),
    Unary(Box<UnaryExpr>),
    Variable(VariableExpr),
    Assign(Box<AssignExpr>),
    Logical(Box<LogicalExpr>),
    Call(Box<CallExpr>),
    Get(Box<GetExpr>),
    Set(Box<SetExpr>),
    This(ThisExpr),
    Super(SuperExpr),
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
            Expr::Super(e) => visitor.visit_super_expr(e),
        }
    }

    pub fn binary(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Binary(Box::new(BinaryExpr {
            left,
            operator,
            right,
        }))
    }

    pub fn grouping(expression: Expr) -> Expr {
        Expr::Grouping(Box::new(GroupingExpr { expression }))
    }

    pub fn literal(value: LoxObject) -> Expr {
        Expr::Literal(LiteralExpr { value })
    }

    pub fn unary(operator: Token, right: Expr) -> Expr {
        Expr::Unary(Box::new(UnaryExpr { operator, right }))
    }

    pub fn variable(name: Token) -> Expr {
        Expr::Variable(VariableExpr::new(name))
    }

    pub fn assign(name: Token, value: Expr, id: usize) -> Expr {
        Expr::Assign(Box::new(AssignExpr { name, value, id }))
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Logical(Box::new(LogicalExpr {
            left,
            operator,
            right,
        }))
    }

    pub fn call(callee: Expr, paren: Token, arguments: Vec<Expr>) -> Expr {
        Expr::Call(Box::new(CallExpr {
            callee,
            paren,
            arguments,
        }))
    }

    pub fn get(name: Token, object: Expr) -> Expr {
        Expr::Get(Box::new(GetExpr { name, object }))
    }

    pub fn set(object: Expr, name: Token, value: Expr) -> Expr {
        Expr::Set(Box::new(SetExpr {
            object,
            name,
            value,
        }))
    }

    pub fn this(keyword: Token) -> Expr {
        Expr::This(ThisExpr { keyword, id: uid() })
    }

    pub fn super_expr(keyword: Token, method: Token) -> Self {
        Expr::Super(SuperExpr {
            keyword,
            method,
            id: uid(),
        })
    }
}

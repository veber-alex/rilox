use crate::object::LoxObject;
use crate::token::Token;
use std::fmt::Debug;

pub trait ExprVisitor {
    type Output;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output;
    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output;
    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output;
    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Self::Output;
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
}

#[derive(Debug)]
pub struct UnboxedAssignExpr {
    pub name: Token,
    pub value: Expr,
}

#[derive(Debug)]
pub struct UnboxedLogicalExpr {
    pub left: Expr,
    pub operator: Token,
    pub right: Expr,
}

pub type BinaryExpr = Box<UnboxedBinaryExpr>;
pub type GroupingExpr = Box<UnboxedGroupingExpr>;
pub type UnaryExpr = Box<UnboxedUnrayExpr>;
pub type AssignExpr = Box<UnboxedAssignExpr>;
pub type LogicalExpr = Box<UnboxedLogicalExpr>;

#[derive(Debug)]
pub enum Expr {
    Binary(BinaryExpr),
    Grouping(GroupingExpr),
    Literal(LiteralExpr),
    Unary(UnaryExpr),
    Variable(VariableExpr),
    Assign(AssignExpr),
    Logical(LogicalExpr),
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
        Expr::Variable(VariableExpr { name })
    }

    pub fn assign(name: Token, value: Expr) -> Expr {
        Expr::Assign(Box::new(UnboxedAssignExpr { name, value }))
    }

    pub fn logical(left: Expr, operator: Token, right: Expr) -> Expr {
        Expr::Logical(Box::new(UnboxedLogicalExpr {
            left,
            operator,
            right,
        }))
    }
}

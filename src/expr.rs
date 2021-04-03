use crate::object::LoxObject;
use crate::token::Token;
use std::fmt::Debug;

pub trait ExprVisitor {
    type Output;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output;
    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output;
    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output;
    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output;
}

#[derive(Debug)]
pub enum Expr {
    Binary(Box<BinaryExpr>),
    Grouping(Box<GroupingExpr>),
    Literal(LiteralExpr),
    Unary(Box<UnaryExpr>),
}

impl Expr {
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

    pub fn accept<V: ExprVisitor>(&self, visitor: &mut V) -> V::Output {
        match self {
            Expr::Binary(e) => visitor.visit_binary_expr(e),
            Expr::Grouping(e) => visitor.visit_grouping_expr(e),
            Expr::Literal(e) => visitor.visit_literal_expr(e),
            Expr::Unary(e) => visitor.visit_unary_expr(e),
        }
    }
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

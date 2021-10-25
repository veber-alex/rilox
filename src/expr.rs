use crate::arena::Id;
use crate::model::object::LoxObject;
use crate::token::Token;
use std::cell::Cell;
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
    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::Output;
    fn visit_get_expr(&mut self, expr: &GetExpr) -> Self::Output;
    fn visit_set_expr(&mut self, expr: &SetExpr) -> Self::Output;
    fn visit_this_expr(&mut self, expr: &ThisExpr) -> Self::Output;
    fn visit_super_expr(&mut self, expr: &SuperExpr) -> Self::Output;
    fn visit_fstring_expr(&mut self, expr: &FstringExpr) -> Self::Output;
}

pub trait ExprHasLocation {
    fn location_cell_ref(&self) -> &Cell<Option<Location>>;

    fn get_location(&self) -> Option<Location> {
        self.location_cell_ref().get()
    }

    fn set_location(&self, distance: usize, index: usize) {
        self.location_cell_ref()
            .set(Some(Location::new(distance, index)))
    }
}

#[derive(Debug, Clone, Copy)]
pub struct Location {
    pub distance: usize,
    pub index: usize,
}

impl Location {
    pub fn new(distance: usize, index: usize) -> Self {
        Self { distance, index }
    }
}

#[derive(Debug)]
pub struct BinaryExpr {
    pub left: Id<Expr>,
    pub operator: Token,
    pub right: Id<Expr>,
}

#[derive(Debug)]
pub struct GroupingExpr {
    pub expression: Id<Expr>,
}

#[derive(Debug)]
pub struct LiteralExpr {
    pub value: LoxObject,
}

#[derive(Debug)]
pub struct UnaryExpr {
    pub operator: Token,
    pub right: Id<Expr>,
}

#[derive(Debug)]
pub struct VariableExpr {
    pub name: Token,
    location: Cell<Option<Location>>,
}

impl VariableExpr {
    pub fn new(name: Token) -> Self {
        Self {
            name,
            location: Default::default(),
        }
    }
}

impl ExprHasLocation for VariableExpr {
    fn location_cell_ref(&self) -> &Cell<Option<Location>> {
        &self.location
    }
}

#[derive(Debug)]
pub struct AssignExpr {
    pub name: Token,
    pub value: Id<Expr>,
    location: Cell<Option<Location>>,
}

impl ExprHasLocation for AssignExpr {
    fn location_cell_ref(&self) -> &Cell<Option<Location>> {
        &self.location
    }
}

#[derive(Debug)]
pub struct CallExpr {
    pub callee: Id<Expr>,
    pub paren: Token,
    pub arguments: Vec<Id<Expr>>,
}

#[derive(Debug)]
pub struct LogicalExpr {
    pub left: Id<Expr>,
    pub operator: Token,
    pub right: Id<Expr>,
}

#[derive(Debug)]
pub struct GetExpr {
    pub name: Token,
    pub object: Id<Expr>,
}

#[derive(Debug)]
pub struct SetExpr {
    pub object: Id<Expr>,
    pub name: Token,
    pub value: Id<Expr>,
}

#[derive(Debug)]
pub struct ThisExpr {
    pub keyword: Token,
    location: Cell<Option<Location>>,
}

impl ExprHasLocation for ThisExpr {
    fn location_cell_ref(&self) -> &Cell<Option<Location>> {
        &self.location
    }
}

#[derive(Debug)]
pub struct SuperExpr {
    pub keyword: Token,
    pub method: Token,
    location: Cell<Option<Location>>,
}

impl ExprHasLocation for SuperExpr {
    fn location_cell_ref(&self) -> &Cell<Option<Location>> {
        &self.location
    }
}

#[derive(Debug)]
pub struct FstringExpr {
    pub string: Vec<Id<Expr>>,
}

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
    Super(SuperExpr),
    Fstring(FstringExpr),
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
            Expr::Fstring(e) => visitor.visit_fstring_expr(e),
        }
    }

    pub fn binary(left: Id<Expr>, operator: Token, right: Id<Expr>) -> Self {
        Expr::Binary(BinaryExpr {
            left,
            operator,
            right,
        })
    }

    pub fn grouping(expression: Id<Expr>) -> Self {
        Expr::Grouping(GroupingExpr { expression })
    }

    pub fn literal(value: LoxObject) -> Self {
        Expr::Literal(LiteralExpr { value })
    }

    pub fn unary(operator: Token, right: Id<Expr>) -> Self {
        Expr::Unary(UnaryExpr { operator, right })
    }

    pub fn variable(name: Token) -> Self {
        Expr::Variable(VariableExpr::new(name))
    }

    pub fn assign(name: Token, value: Id<Expr>) -> Self {
        Expr::Assign(AssignExpr {
            name,
            value,
            location: Default::default(),
        })
    }

    pub fn logical(left: Id<Expr>, operator: Token, right: Id<Expr>) -> Self {
        Expr::Logical(LogicalExpr {
            left,
            operator,
            right,
        })
    }

    pub fn call(callee: Id<Expr>, paren: Token, arguments: Vec<Id<Expr>>) -> Self {
        Expr::Call(CallExpr {
            callee,
            paren,
            arguments,
        })
    }

    pub fn get(name: Token, object: Id<Expr>) -> Self {
        Expr::Get(GetExpr { name, object })
    }

    pub fn set(object: Id<Expr>, name: Token, value: Id<Expr>) -> Self {
        Expr::Set(SetExpr {
            object,
            name,
            value,
        })
    }

    pub fn this(keyword: Token) -> Self {
        Expr::This(ThisExpr {
            keyword,
            location: Default::default(),
        })
    }

    pub fn super_expr(keyword: Token, method: Token) -> Self {
        Expr::Super(SuperExpr {
            keyword,
            method,
            location: Default::default(),
        })
    }

    pub fn fstring(string: Vec<Id<Expr>>) -> Self {
        Expr::Fstring(FstringExpr { string })
    }
}

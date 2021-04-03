use crate::expr::Expr;
use crate::token::Token;

pub trait StmtVisitor {
    type Output;

    fn visit_expr_stmt(&mut self, stmt: ExprStmt) -> Self::Output;
    fn visit_print_stmt(&mut self, stmt: PrintStmt) -> Self::Output;
    fn visit_var_stmt(&mut self, stmt: VarStmt) -> Self::Output;
}

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    Var(VarStmt),
}

impl Stmt {
    pub fn accept<V: StmtVisitor>(self, visitor: &mut V) -> V::Output {
        match self {
            Stmt::Expr(s) => visitor.visit_expr_stmt(s),
            Stmt::Print(s) => visitor.visit_print_stmt(s),
            Stmt::Var(s) => visitor.visit_var_stmt(s),
        }
    }

    pub fn expr(expression: Expr) -> Stmt {
        Stmt::Expr(ExprStmt { expression })
    }

    pub fn print(expression: Expr) -> Stmt {
        Stmt::Print(PrintStmt { expression })
    }

    pub fn var(name: Token, initializer: Option<Expr>) -> Stmt {
        Stmt::Var(VarStmt { name, initializer })
    }
}

#[derive(Debug)]
pub struct ExprStmt {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct PrintStmt {
    pub expression: Expr,
}

#[derive(Debug)]
pub struct VarStmt {
    pub name: Token,
    pub initializer: Option<Expr>,
}

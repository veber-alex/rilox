use crate::expr::Expr;
use crate::token::Token;

pub trait StmtVisitor {
    type Output;

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output;
    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Self::Output;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::Output;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::Output;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output;
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

#[derive(Debug)]
pub struct BlockStmt {
    pub statements: Vec<Stmt>,
}

#[derive(Debug)]
pub struct UnboxedIfStmt {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

#[derive(Debug)]
pub struct UnboxedWhileStmt {
    pub condition: Expr,
    pub body: Stmt,
}

pub type IfStmt = Box<UnboxedIfStmt>;
pub type WhileStmt = Box<UnboxedWhileStmt>;

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    Var(VarStmt),
    Block(BlockStmt),
    If(IfStmt),
    While(WhileStmt),
}

impl Stmt {
    pub fn accept<V: StmtVisitor>(&self, visitor: &mut V) -> V::Output {
        match self {
            Stmt::Expr(s) => visitor.visit_expr_stmt(s),
            Stmt::Print(s) => visitor.visit_print_stmt(s),
            Stmt::Var(s) => visitor.visit_var_stmt(s),
            Stmt::Block(s) => visitor.visit_block_stmt(s),
            Stmt::If(s) => visitor.visit_if_stmt(s),
            Stmt::While(s) => visitor.visit_while_stmt(s),
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

    pub fn block(statements: Vec<Stmt>) -> Stmt {
        Stmt::Block(BlockStmt { statements })
    }

    pub fn if_else(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Stmt {
        Stmt::If(Box::new(UnboxedIfStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }

    pub fn while_loop(condition: Expr, body: Stmt) -> Stmt {
        Stmt::While(Box::new(UnboxedWhileStmt { condition, body }))
    }
}

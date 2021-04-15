use crate::expr::{Expr, VariableExpr};
use crate::token::Token;
use std::rc::Rc;

pub trait StmtVisitor {
    type Output;

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output;
    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output;
    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Self::Output;
    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::Output;
    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::Output;
    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output;
    fn visit_break_stmt(&mut self, stmt: &BreakStmt) -> Self::Output;
    fn visit_function_stmt(&mut self, stmt: &FunStmt) -> Self::Output;
    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::Output;
    fn visit_class_stmt(&mut self, stmt: &ClassStmt) -> Self::Output;
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
pub struct IfStmt {
    pub condition: Expr,
    pub then_branch: Stmt,
    pub else_branch: Option<Stmt>,
}

#[derive(Debug)]
pub struct WhileStmt {
    pub condition: Expr,
    pub body: Stmt,
}

#[derive(Debug)]
pub struct BreakStmt {
    pub keyword: Token,
}

#[derive(Debug)]
pub struct UnboxedFunStmt {
    pub name: Token,
    pub params: Vec<Token>,
    pub body: Vec<Stmt>,
}

impl PartialEq for UnboxedFunStmt {
    fn eq(&self, other: &Self) -> bool {
        self.name.lexeme == other.name.lexeme
    }
}

#[derive(Debug)]
pub struct ReturnStmt {
    pub keyword: Token,
    pub value: Option<Expr>,
}

#[derive(Debug)]
pub struct ClassStmt {
    pub name: Token,
    pub methods: Vec<FunStmt>,
    pub superclass: Option<VariableExpr>,
}

pub type FunStmt = Rc<UnboxedFunStmt>;

#[derive(Debug)]
pub enum Stmt {
    Expr(ExprStmt),
    Print(PrintStmt),
    Var(VarStmt),
    Block(BlockStmt),
    If(Box<IfStmt>),
    While(Box<WhileStmt>),
    Break(BreakStmt),
    Fun(FunStmt),
    Return(ReturnStmt),
    Class(ClassStmt),
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
            Stmt::Break(s) => visitor.visit_break_stmt(s),
            Stmt::Fun(s) => visitor.visit_function_stmt(s),
            Stmt::Return(s) => visitor.visit_return_stmt(s),
            Stmt::Class(s) => visitor.visit_class_stmt(s),
        }
    }

    pub fn expr(expression: Expr) -> Stmt {
        Stmt::Expr(ExprStmt { expression })
    }

    pub fn print(expression: Expr) -> Stmt {
        Stmt::Print(PrintStmt { expression })
    }

    pub fn return_stmt(keyword: Token, value: Option<Expr>) -> Self {
        Stmt::Return(ReturnStmt { keyword, value })
    }

    pub fn var(name: Token, initializer: Option<Expr>) -> Stmt {
        Stmt::Var(VarStmt { name, initializer })
    }

    pub fn block(statements: Vec<Stmt>) -> Stmt {
        Stmt::Block(BlockStmt { statements })
    }

    pub fn if_else(condition: Expr, then_branch: Stmt, else_branch: Option<Stmt>) -> Stmt {
        Stmt::If(Box::new(IfStmt {
            condition,
            then_branch,
            else_branch,
        }))
    }

    pub fn while_loop(condition: Expr, body: Stmt) -> Stmt {
        Stmt::While(Box::new(WhileStmt { condition, body }))
    }

    pub fn break_stmt(keyword: Token) -> Stmt {
        Stmt::Break(BreakStmt { keyword })
    }

    pub fn function_stmt(name: Token, params: Vec<Token>, body: Vec<Stmt>) -> FunStmt {
        Rc::new(UnboxedFunStmt { name, params, body })
    }

    pub fn function(fun_stmt: FunStmt) -> Stmt {
        Stmt::Fun(fun_stmt)
    }

    pub fn class(name: Token, methods: Vec<FunStmt>, superclass: Option<VariableExpr>) -> Self {
        Stmt::Class(ClassStmt {
            name,
            methods,
            superclass,
        })
    }
}

#[derive(Debug)]
pub enum Stmt {
    ExprStmt(ExprStmt),
    PrintStmt(PrintStmt),
}

#[derive(Debug)]
pub struct ExprStmt {}

#[derive(Debug)]
pub struct PrintStmt {}

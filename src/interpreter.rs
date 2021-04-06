use crate::enviroment::EnviromentStack;
use crate::expr::{
    AssignExpr, BinaryExpr, Expr, ExprVisitor, GroupingExpr, LiteralExpr, UnaryExpr, VariableExpr,
};
use crate::object::LoxObject;
use crate::report_error;
use crate::stmt::{BlockStmt, ExprStmt, IfStmt, PrintStmt, Stmt, StmtVisitor, VarStmt};
use crate::token::TokenType::*;

#[derive(Debug, Default)]
pub struct Interpreter {
    env_stack: EnviromentStack,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Option<()> {
        for stmt in statements {
            self.execute(stmt).map_err(|e| e.report()).ok()?;
        }

        Some(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), RuntimeError> {
        stmt.accept(self)
    }

    fn execute_block(&mut self, statements: &BlockStmt) -> Result<(), RuntimeError> {
        self.env_stack.push();
        for stmt in &statements.statements {
            self.execute(stmt)?;
        }
        self.env_stack.pop();

        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        expr.accept(self)
    }

    fn is_truthy(object: &LoxObject) -> bool {
        match object {
            LoxObject::Nil => false,
            LoxObject::Bool(b) => b.0,
            _ => true,
        }
    }
}

impl ExprVisitor for Interpreter {
    type Output = Result<LoxObject, RuntimeError>;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.ttype {
            BangEqual => Ok(LoxObject::bool(left != right)),
            EqualEqual => Ok(LoxObject::bool(left == right)),
            Minus | Slash | Star | Greater | GreaterEqual | Less | LessEqual => {
                if let (LoxObject::Number(lvalue), LoxObject::Number(rvalue)) = (left, right) {
                    Ok(match expr.operator.ttype {
                        Minus => LoxObject::number(lvalue.0 - rvalue.0),
                        Slash => LoxObject::number(lvalue.0 / rvalue.0),
                        Star => LoxObject::number(lvalue.0 * rvalue.0),
                        Greater => LoxObject::bool(lvalue.0 > rvalue.0),
                        GreaterEqual => LoxObject::bool(lvalue.0 >= rvalue.0),
                        Less => LoxObject::bool(lvalue.0 < rvalue.0),
                        LessEqual => LoxObject::bool(lvalue.0 <= rvalue.0),
                        _ => unreachable!(),
                    })
                } else {
                    Err(RuntimeError::new(
                        expr.operator.line,
                        "Operands must be numbers.".into(),
                    ))
                }
            }
            Plus => match (left, right) {
                (LoxObject::Number(lvalue), LoxObject::Number(rvalue)) => {
                    Ok(LoxObject::number(lvalue.0 + rvalue.0))
                }
                (LoxObject::String(lvalue), LoxObject::String(rvalue)) => {
                    Ok(LoxObject::string(lvalue.0.as_ref().to_owned() + &rvalue.0))
                }
                _ => Err(RuntimeError::new(
                    expr.operator.line,
                    "Operands must be two numbers or two strings.".into(),
                )),
            },
            _ => unreachable!(),
        }
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output {
        self.evaluate(&expr.expression)
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output {
        Ok(expr.value.clone())
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output {
        let right = self.evaluate(&expr.right)?;
        match expr.operator.ttype {
            Minus => {
                if let LoxObject::Number(value) = right {
                    Ok(LoxObject::number(-value.0))
                } else {
                    Err(RuntimeError::new(
                        expr.operator.line,
                        format!("type {} cannot be negated", right.type_as_str()),
                    ))
                }
            }
            Bang => Ok(LoxObject::bool(!Self::is_truthy(&right))),
            _ => unreachable!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output {
        self.env_stack.get(&expr.name)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output {
        let value = self.evaluate(&expr.value)?;
        self.env_stack.assign(&expr.name, value.clone())?;
        Ok(value)
    }
}

impl StmtVisitor for Interpreter {
    type Output = Result<(), RuntimeError>;

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output {
        self.evaluate(&stmt.expression)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output {
        let value = self.evaluate(&stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Self::Output {
        let initializer = if let Some(expr) = &stmt.initializer {
            self.evaluate(expr)?
        } else {
            LoxObject::nil()
        };

        // FIXME: Remove this clone
        self.env_stack.define(stmt.name.lexeme.clone(), initializer);
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::Output {
        self.execute_block(stmt)
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::Output {
        let condition = self.evaluate(&stmt.condition)?;
        if Self::is_truthy(&condition) {
            self.execute(&stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    // TODO: Span type to replace line
    line: usize,
    msg: String,
}

impl RuntimeError {
    // FIXME: Use function to create errors and Cow
    pub fn new(line: usize, msg: String) -> Self {
        Self { line, msg }
    }

    fn report(self) -> Self {
        report_error("Runtime", self.line, "", &self.msg);
        self
    }
}

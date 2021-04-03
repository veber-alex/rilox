use crate::enviroment::Enviroment;
use crate::expr::{
    BinaryExpr, Expr, ExprVisitor, GroupingExpr, LiteralExpr, UnaryExpr, VariableExpr,
};
use crate::object::{LoxBool, LoxNumber, LoxString};
use crate::object::{LoxNil, LoxObject};
use crate::report_error;
use crate::stmt::{ExprStmt, PrintStmt, Stmt, StmtVisitor, VarStmt};
use crate::token::TokenType::*;

#[derive(Debug, Default)]
pub struct Interpreter {
    enviroment: Enviroment,
}

impl Interpreter {
    pub fn new() -> Self {
        Default::default()
    }

    pub fn interpret(&mut self, statements: Vec<Stmt>) -> Option<()> {
        for stmt in statements {
            self.execute(stmt).ok()?;
        }

        Some(())
    }

    fn execute(&mut self, stmt: Stmt) -> Result<(), RuntimeError> {
        stmt.accept(self)
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject, RuntimeError> {
        expr.accept(self)
    }

    fn is_truthy(object: &LoxObject) -> bool {
        if object.as_any().downcast_ref::<LoxNil>().is_some() {
            return false;
        }

        if let Some(value) = object.as_any().downcast_ref::<LoxBool>() {
            return value.0;
        }

        true
    }

    fn is_equal(left: &LoxObject, right: &LoxObject) -> bool {
        left.hash() == right.hash()
    }

    fn error(line: usize, msg: String) -> RuntimeError {
        report_error(line, "", &msg);
        RuntimeError::new(line, msg)
    }
}

impl ExprVisitor for Interpreter {
    type Output = Result<LoxObject, RuntimeError>;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.ttype {
            BangEqual => Ok(LoxBool::new(!Self::is_equal(&left, &right))),
            EqualEqual => Ok(LoxBool::new(Self::is_equal(&left, &right))),
            Minus | Slash | Star | Greater | GreaterEqual | Less | LessEqual => {
                if let (Some(lvalue), Some(rvalue)) = (
                    left.as_any().downcast_ref::<LoxNumber>(),
                    right.as_any().downcast_ref::<LoxNumber>(),
                ) {
                    let ret = match expr.operator.ttype {
                        Minus => LoxNumber::new(lvalue.0 - rvalue.0),
                        Slash => LoxNumber::new(lvalue.0 / rvalue.0),
                        Star => LoxNumber::new(lvalue.0 * rvalue.0),
                        Greater => LoxBool::new(lvalue.0 > rvalue.0),
                        GreaterEqual => LoxBool::new(lvalue.0 >= rvalue.0),
                        Less => LoxBool::new(lvalue.0 < rvalue.0),
                        LessEqual => LoxBool::new(lvalue.0 <= rvalue.0),
                        _ => unreachable!(),
                    };
                    Ok(ret)
                } else {
                    Err(Self::error(
                        expr.operator.line(),
                        "Operands must be numbers.".into(),
                    ))
                }
            }
            Plus => {
                if let (Some(lvalue), Some(rvalue)) = (
                    left.as_any().downcast_ref::<LoxNumber>(),
                    right.as_any().downcast_ref::<LoxNumber>(),
                ) {
                    Ok(LoxNumber::new(lvalue.0 + rvalue.0))
                } else if let (Some(lvalue), Some(rvalue)) = (
                    left.as_any().downcast_ref::<LoxString>(),
                    right.as_any().downcast_ref::<LoxString>(),
                ) {
                    Ok(LoxString::new(lvalue.0.clone() + &rvalue.0))
                } else {
                    Err(Self::error(
                        expr.operator.line(),
                        "Operands must be two numbers or two strings.".into(),
                    ))
                }
            }
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
                if let Some(value) = right.as_any().downcast_ref::<LoxNumber>() {
                    Ok(LoxNumber::new(-value.0))
                } else {
                    Err(Self::error(
                        expr.operator.line(),
                        format!("type {} cannot be negated", right.type_as_str()),
                    ))
                }
            }
            Bang => Ok(LoxBool::new(!Self::is_truthy(&right))),
            _ => unreachable!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output {
        todo!()
    }
}

impl StmtVisitor for Interpreter {
    type Output = Result<(), RuntimeError>;

    fn visit_expr_stmt(&mut self, stmt: ExprStmt) -> Self::Output {
        self.evaluate(&stmt.expression)?;
        Ok(())
    }

    fn visit_print_stmt(&mut self, stmt: PrintStmt) -> Self::Output {
        let value = self.evaluate(&stmt.expression)?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: VarStmt) -> Self::Output {
        let initializer = if let Some(expr) = stmt.initializer {
            self.evaluate(&expr)?
        } else {
            LoxNil::new()
        };

        self.enviroment.define(stmt.name.lexeme, initializer);
        Ok(())
    }
}

#[derive(Debug)]
pub struct RuntimeError {
    line: usize,
    msg: String,
}

impl RuntimeError {
    pub fn new(line: usize, msg: String) -> Self {
        Self { line, msg }
    }
}

use crate::enviroment::Enviroment;
use crate::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, ExprVisitor, GetExpr, GroupingExpr, LiteralExpr,
    LogicalExpr, SetExpr, ThisExpr, UnaryExpr, VariableExpr,
};
use crate::model::callable::LoxCallable;
use crate::model::function::LoxFunction;
use crate::model::object::LoxObject;
use crate::report_error;
use crate::stmt::{
    BlockStmt, BreakStmt, ClassStmt, ExprStmt, FunStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
    StmtVisitor, VarStmt, WhileStmt,
};
use crate::token::{Token, TokenType::*};
use std::collections::HashMap;
use std::mem;

#[derive(Debug, Default)]
pub struct Interpreter {
    environment: Enviroment,
    pub globals: Enviroment,
    locals: HashMap<usize, usize>,
}

impl Interpreter {
    pub fn new() -> Self {
        let environment = Enviroment::default();
        environment.define(
            "clock".to_string(),
            LoxObject::callable(LoxCallable::clock()),
        );

        Self {
            globals: environment.clone(),
            environment,
            ..Default::default()
        }
    }

    pub fn interpret(&mut self, statements: &[Stmt]) -> Option<()> {
        for stmt in statements {
            self.execute(stmt).map_err(|e| e.report()).ok()?;
        }

        Some(())
    }

    fn execute(&mut self, stmt: &Stmt) -> Result<(), ControlFlow> {
        stmt.accept(self)
    }

    pub fn execute_block(
        &mut self,
        statements: &[Stmt],
        env: Enviroment,
    ) -> Result<(), ControlFlow> {
        let previous = mem::replace(&mut self.environment, env);

        for stmt in statements {
            if let err @ Err(_) = self.execute(stmt) {
                self.environment = previous;
                return err;
            }
        }

        self.environment = previous;

        Ok(())
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject, ControlFlow> {
        expr.accept(self)
    }

    fn is_truthy(object: &LoxObject) -> bool {
        match object {
            LoxObject::Nil => false,
            LoxObject::Bool(b) => *b,
            _ => true,
        }
    }

    pub fn resolve(&mut self, id: usize, distance: usize) {
        self.locals.insert(id, distance);
    }

    fn lookup_variable(&mut self, id: usize, name: &Token) -> Result<LoxObject, ControlFlow> {
        if let Some(distance) = self.locals.get(&id) {
            self.environment.get_at(*distance, &name.lexeme)
        } else {
            self.globals.get(name)
        }
    }
}

impl ExprVisitor for Interpreter {
    type Output = Result<LoxObject, ControlFlow>;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.ttype {
            BangEqual => Ok(LoxObject::bool(left != right)),
            EqualEqual => Ok(LoxObject::bool(left == right)),
            Minus | Slash | Star | Greater | GreaterEqual | Less | LessEqual => {
                if let (LoxObject::Number(lvalue), LoxObject::Number(rvalue)) = (left, right) {
                    Ok(match expr.operator.ttype {
                        Minus => LoxObject::number(lvalue - rvalue),
                        Slash => LoxObject::number(lvalue / rvalue),
                        Star => LoxObject::number(lvalue * rvalue),
                        Greater => LoxObject::bool(lvalue > rvalue),
                        GreaterEqual => LoxObject::bool(lvalue >= rvalue),
                        Less => LoxObject::bool(lvalue < rvalue),
                        LessEqual => LoxObject::bool(lvalue <= rvalue),
                        _ => unreachable!(),
                    })
                } else {
                    Err(ControlFlow::abort(
                        expr.operator.line,
                        "Operands must be numbers.".into(),
                    ))
                }
            }
            Plus => match (left, right) {
                (LoxObject::Number(lvalue), LoxObject::Number(rvalue)) => {
                    Ok(LoxObject::number(lvalue + rvalue))
                }
                (LoxObject::String(lvalue), LoxObject::String(rvalue)) => {
                    Ok(LoxObject::string(lvalue.as_ref().to_owned() + &rvalue))
                }
                _ => Err(ControlFlow::abort(
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
                    Ok(LoxObject::number(-value))
                } else {
                    Err(ControlFlow::abort(
                        expr.operator.line,
                        "Only nunbers can be negated".to_string(),
                    ))
                }
            }
            Bang => Ok(LoxObject::bool(!Self::is_truthy(&right))),
            _ => unreachable!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output {
        self.lookup_variable(expr.id, &expr.name)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output {
        let value = self.evaluate(&expr.value)?;
        if let Some(distance) = self.locals.get(&expr.id) {
            self.environment
                .assign_at(*distance, &expr.name, value.clone())?;
        } else {
            self.globals.assign(&expr.name, value.clone())?;
        }

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Self::Output {
        let left = self.evaluate(&expr.left)?;

        if (expr.operator.ttype == Or && Self::is_truthy(&left))
            || (expr.operator.ttype == And && !Self::is_truthy(&left))
        {
            Ok(left)
        } else {
            self.evaluate(&expr.right)
        }
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::Output {
        let callee = self.evaluate(&expr.callee)?;
        let arguments = expr
            .arguments
            .iter()
            .map(|arg| self.evaluate(arg))
            .collect::<Result<Vec<_>, _>>()?;

        if let LoxObject::Callable(callable) = callee {
            let args_len = arguments.len();
            let arity = callable.arity();
            if args_len == arity {
                callable.call(self, arguments)
            } else {
                Err(ControlFlow::abort(
                    expr.paren.line,
                    format!("Expected {} arguments but got {}.", arity, args_len),
                ))
            }
        } else {
            Err(ControlFlow::abort(
                expr.paren.line,
                "Can only call functions and classes.".to_string(),
            ))
        }
    }

    fn visit_get_expr(&mut self, expr: &GetExpr) -> Self::Output {
        let object = self.evaluate(&expr.object)?;
        if let LoxObject::Instance(instance) = object {
            instance.get(&expr.name)
        } else {
            Err(ControlFlow::abort(
                expr.name.line,
                "Only instances have properties.".to_string(),
            ))
        }
    }

    fn visit_set_expr(&mut self, expr: &SetExpr) -> Self::Output {
        let object = self.evaluate(&expr.object)?;

        if let LoxObject::Instance(instance) = object {
            let value = self.evaluate(&expr.value)?;
            instance.set(&expr.name, value.clone());
            Ok(value)
        } else {
            Err(ControlFlow::abort(
                expr.name.line,
                "Only instances have fields.".to_string(),
            ))
        }
    }

    fn visit_this_expr(&mut self, expr: &ThisExpr) -> Self::Output {
        self.lookup_variable(expr.id, &expr.keyword)
    }
}

impl StmtVisitor for Interpreter {
    type Output = Result<(), ControlFlow>;

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
        let initializer = stmt
            .initializer
            .as_ref()
            .map_or_else(|| Ok(LoxObject::nil()), |expr| self.evaluate(expr))?;

        // FIXME: Remove this clone
        self.environment
            .define(stmt.name.lexeme.clone(), initializer);
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::Output {
        self.execute_block(
            &stmt.statements,
            Enviroment::with_enclosing(self.environment.clone()),
        )
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

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output {
        while Self::is_truthy(&self.evaluate(&stmt.condition)?) {
            match self.execute(&stmt.body) {
                Err(ControlFlow::Break) => return Ok(()),
                err @ Err(_) => return err,
                Ok(_) => (),
            }
        }

        Ok(())
    }

    fn visit_break_stmt(&mut self, _stmt: &BreakStmt) -> Self::Output {
        Err(ControlFlow::Break)
    }

    fn visit_function_stmt(&mut self, stmt: &FunStmt) -> Self::Output {
        let function = LoxObject::callable(LoxCallable::function(
            stmt.clone(),
            self.environment.clone(),
            false,
        ));
        self.environment.define(stmt.name.lexeme.clone(), function);
        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::Output {
        let value = stmt.value.as_ref().map(|e| self.evaluate(e)).transpose()?;
        Err(ControlFlow::return_(value))
    }

    fn visit_class_stmt(&mut self, stmt: &ClassStmt) -> Self::Output {
        self.environment
            .define(stmt.name.lexeme.clone(), LoxObject::nil());

        let mut methods = HashMap::new();
        for method in &stmt.methods {
            let is_initializer = method.name.lexeme == "init";
            let function =
                LoxFunction::new(method.clone(), self.environment.clone(), is_initializer);
            methods.insert(method.name.lexeme.clone(), function);
        }

        let class = LoxObject::callable(LoxCallable::class(stmt.name.lexeme.clone(), methods));
        self.environment.assign(&stmt.name, class)?;

        Ok(())
    }
}

#[derive(Debug)]
pub enum ControlFlow {
    // TODO: Span type to replace line
    Abort { line: usize, msg: String },
    Break,
    Return(Option<LoxObject>),
}

impl ControlFlow {
    // FIXME: Use function to create errors and Cow
    pub fn abort(line: usize, msg: String) -> Self {
        Self::Abort { line, msg }
    }

    pub fn return_(value: Option<LoxObject>) -> Self {
        Self::Return(value)
    }

    fn report(self) -> Self {
        if let Self::Abort { line, msg } = &self {
            report_error("Runtime", *line, "", msg);
        }
        self
    }
}

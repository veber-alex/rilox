use crate::enviroment::Enviroment;
use crate::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, ExprVisitor, GetExpr, GroupingExpr, LiteralExpr,
    LogicalExpr, SetExpr, SuperExpr, ThisExpr, UnaryExpr, VariableExpr,
};
use crate::model::callable::LoxCallable;
use crate::model::function::LoxFunction;
use crate::model::object::LoxObject;
use crate::report_error;
use crate::stmt::{
    BlockStmt, BreakStmt, ClassStmt, ExprStmt, FunStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
    StmtVisitor, VarStmt, WhileStmt,
};
use crate::token::{Token, TokenKind::*};
use std::collections::HashMap;
use std::mem;

#[derive(Debug, Default)]
pub struct Interpreter {
    environment: Enviroment,
    vars: HashMap<usize, (usize, usize)>,
}

impl Interpreter {
    pub fn new() -> Self {
        let environment = Enviroment::default();
        // environment.define(
        //     "clock".to_string(),
        //     LoxObject::callable(LoxCallable::clock()),
        // );

        Self {
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
        let result = statements.iter().try_for_each(|stmt| self.execute(stmt));
        self.environment = previous;

        result
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject, ControlFlow> {
        expr.accept(self)
    }

    fn evaluate_variable(&mut self, expr: &VariableExpr) -> Result<LoxObject, ControlFlow> {
        self.visit_variable_expr(expr)
    }

    pub fn resolve(&mut self, id: usize, distance: usize, index: usize) {
        self.vars.insert(id, (distance, index));
    }

    fn lookup_variable(&mut self, id: usize, name: &Token) -> Result<LoxObject, ControlFlow> {
        if let Some(&(distance, index)) = self.vars.get(&id) {
            Ok(self.environment.get_at(distance, index))
        } else {
            Err(ControlFlow::abort(
                name.line,
                format!("Undefined variable '{}'.", name.lexeme),
            ))
        }
    }
}

impl ExprVisitor for Interpreter {
    type Output = Result<LoxObject, ControlFlow>;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        let left = self.evaluate(&expr.left)?;
        let right = self.evaluate(&expr.right)?;

        match expr.operator.kind {
            BangEqual => Ok(LoxObject::bool(left != right)),
            EqualEqual => Ok(LoxObject::bool(left == right)),
            Minus | Slash | Star | Greater | GreaterEqual | Less | LessEqual => {
                if let (LoxObject::Number(lvalue), LoxObject::Number(rvalue)) = (left, right) {
                    Ok(match expr.operator.kind {
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
                    Ok(LoxObject::string(&format!("{}{}", lvalue, rvalue)))
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
        match expr.operator.kind {
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
            Bang => Ok(LoxObject::bool(!right.is_truthy())),
            _ => unreachable!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output {
        self.lookup_variable(expr.id, &expr.name)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output {
        let value = self.evaluate(&expr.value)?;
        if let Some(&(distance, index)) = self.vars.get(&expr.id) {
            self.environment.assign_at(distance, index, value.clone());
        } else {
            return Err(ControlFlow::abort(
                expr.name.line,
                format!("Undefined variable '{}'.", expr.name.lexeme),
            ));
        }

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Self::Output {
        let left = self.evaluate(&expr.left)?;

        if (expr.operator.kind == Or && left.is_truthy())
            || (expr.operator.kind == And && !left.is_truthy())
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

    fn visit_super_expr(&mut self, expr: &SuperExpr) -> Self::Output {
        let &(distance, index) = self
            .vars
            .get(&expr.id)
            .expect("super in class without super");

        let superclass = match self.environment.get_at(distance, index) {
            LoxObject::Callable(LoxCallable::Class(cls)) => cls,
            _ => panic!("super outside of class"),
        };

        let instance = match self.environment.get_at(distance - 1, 0) {
            LoxObject::Instance(inst) => inst,
            _ => panic!("unbound method"),
        };

        let method = superclass.find_method(&expr.method.lexeme).ok_or_else(|| {
            ControlFlow::abort(
                expr.method.line,
                format!("Undefined property '{}'", expr.method.lexeme),
            )
        })?;

        Ok(LoxObject::Callable(LoxCallable::function_from(
            method.bind(instance),
        )))
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

        self.environment.define(initializer);
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
        if condition.is_truthy() {
            self.execute(&stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output {
        while self.evaluate(&stmt.condition)?.is_truthy() {
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
        self.environment.define(function);

        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::Output {
        let value = stmt.value.as_ref().map(|e| self.evaluate(e)).transpose()?;
        Err(ControlFlow::return_(value))
    }

    fn visit_class_stmt(&mut self, stmt: &ClassStmt) -> Self::Output {
        let superclass = if let Some(var_expr) = &stmt.superclass {
            if let LoxObject::Callable(LoxCallable::Class(superclass)) =
                self.evaluate_variable(var_expr)?
            {
                Some(superclass)
            } else {
                return Err(ControlFlow::abort(
                    var_expr.name.line,
                    "Superclass must be a class.".into(),
                ));
            }
        } else {
            None
        };

        let index = self.environment.define(LoxObject::nil());

        let enclosing = if let Some(superclass) = &superclass {
            let enclosing = self.environment.clone();
            self.environment = Enviroment::with_enclosing(enclosing.clone());
            self.environment
                .define(LoxObject::callable(LoxCallable::class_from(
                    superclass.clone(),
                )));
            Some(enclosing)
        } else {
            None
        };

        let mut methods = HashMap::new();
        for method in &stmt.methods {
            let is_initializer = method.name.lexeme == "init";
            let function =
                LoxFunction::new(method.clone(), self.environment.clone(), is_initializer);
            methods.insert(method.name.lexeme.clone(), function);
        }

        let class = LoxObject::callable(LoxCallable::class(
            stmt.name.lexeme.clone(),
            superclass,
            methods,
        ));

        if let Some(enclosing) = enclosing {
            self.environment = enclosing
        }

        self.environment.assign_at(0, index, class);

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

use crate::context::Context;
use crate::environment::Environment;
use crate::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, ExprHasLocation, ExprVisitor, FstringExpr, GetExpr,
    GroupingExpr, LiteralExpr, Location, LogicalExpr, SetExpr, SuperExpr, ThisExpr, UnaryExpr,
    VariableExpr,
};
use crate::model::callable::LoxCallable;
use crate::model::function::LoxFunction;
use crate::model::object::{LoxObject, LoxObjectEnum};
use crate::report_error;
use crate::stmt::{
    BlockStmt, BreakStmt, ClassStmt, ExprStmt, FunStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
    StmtVisitor, VarStmt, WhileStmt,
};
use crate::token::{Token, TokenKind::*};

use qcell::{TCell, TCellOwner};
use rustc_hash::FxHashMap;

use std::mem;

// QCell types
pub struct TCellMarker;
pub type ACell<T> = TCell<TCellMarker, T>;
pub type ACellOwner = TCellOwner<TCellMarker>;

pub struct Interpreter<'a> {
    pub environment: Environment,
    pub arguments_buffer: Vec<LoxObject>,
    pub acell_owner: ACellOwner,
    ctx: &'a Context,
}

impl<'a> Interpreter<'a> {
    pub fn new(ctx: &'a Context) -> Self {
        Self {
            environment: Default::default(),
            arguments_buffer: Default::default(),
            acell_owner: Default::default(),
            ctx,
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
        env: Environment,
    ) -> Result<(), ControlFlow> {
        let previous = mem::replace(&mut self.environment, env);
        let result = statements.iter().try_for_each(|stmt| self.execute(stmt));
        self.environment = previous;

        result
    }

    fn evaluate(&mut self, expr: &Expr) -> Result<LoxObject, ControlFlow> {
        expr.accept(self)
    }

    fn lookup_variable<EXPR>(&mut self, expr: &EXPR, name: &Token) -> Result<LoxObject, ControlFlow>
    where
        EXPR: ExprHasLocation,
    {
        expr.get_location()
            .ok_or_else(|| {
                ControlFlow::abort(name.line, format!("Undefined variable '{}'.", name.lexeme))
            })
            .map(|loc| self.environment.get_at(loc, &self.acell_owner))
    }
}

impl ExprVisitor for Interpreter<'_> {
    type Output = Result<LoxObject, ControlFlow>;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        let left = match self.evaluate(self.ctx.expr.get(expr.left)) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };
        let right = match self.evaluate(self.ctx.expr.get(expr.right)) {
            Ok(v) => v,
            Err(e) => return Err(e),
        };

        match expr.operator.kind {
            BangEqual => Ok(LoxObject::bool(left != right)),
            EqualEqual => Ok(LoxObject::bool(left == right)),
            Minus | Slash | Star | Greater | GreaterEqual | Less | LessEqual => {
                if let (LoxObjectEnum::Number(lvalue), LoxObjectEnum::Number(rvalue)) =
                    (&*left, &*right)
                {
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
            Plus => match (&*left, &*right) {
                // 2 numbers
                (LoxObjectEnum::Number(lvalue), LoxObjectEnum::Number(rvalue)) => {
                    Ok(LoxObject::number(lvalue + rvalue))
                }
                // 2 dynamic strings
                (LoxObjectEnum::String(lvalue), LoxObjectEnum::String(rvalue)) => {
                    Ok(LoxObject::string(format!("{}{}", lvalue, rvalue)))
                }
                // 2 static strings
                (LoxObjectEnum::StaticString(lvalue), LoxObjectEnum::StaticString(rvalue)) => {
                    Ok(LoxObject::string(format!("{}{}", lvalue, rvalue)))
                }
                // dynamic + static string
                (LoxObjectEnum::String(lvalue), LoxObjectEnum::StaticString(rvalue)) => {
                    Ok(LoxObject::string(format!("{}{}", lvalue, rvalue)))
                }
                // static + dynamic string
                (LoxObjectEnum::StaticString(lvalue), LoxObjectEnum::String(rvalue)) => {
                    Ok(LoxObject::string(format!("{}{}", lvalue, rvalue)))
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
        self.evaluate(self.ctx.expr.get(expr.expression))
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output {
        Ok(expr.value.clone())
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output {
        let right = self.evaluate(self.ctx.expr.get(expr.right))?;
        match expr.operator.kind {
            Minus => {
                if let LoxObjectEnum::Number(value) = &*right.0 {
                    Ok(LoxObject::number(-value))
                } else {
                    Err(ControlFlow::abort(
                        expr.operator.line,
                        "Only numbers can be negated".to_string(),
                    ))
                }
            }
            Bang => Ok(LoxObject::bool(!right.is_truthy())),
            _ => unreachable!(),
        }
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output {
        self.lookup_variable(expr, &expr.name)
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output {
        let value = self.evaluate(self.ctx.expr.get(expr.value))?;
        if let Some(loc) = expr.get_location() {
            self.environment
                .assign_at(loc, value.clone(), &mut self.acell_owner);
        } else {
            return Err(ControlFlow::abort(
                expr.name.line,
                format!("Undefined variable '{}'.", expr.name.lexeme),
            ));
        }

        Ok(value)
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Self::Output {
        let left = self.evaluate(self.ctx.expr.get(expr.left))?;

        if (expr.operator.kind == Or && left.is_truthy())
            || (expr.operator.kind == And && !left.is_truthy())
        {
            Ok(left)
        } else {
            self.evaluate(self.ctx.expr.get(expr.right))
        }
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::Output {
        let callee = self.evaluate(self.ctx.expr.get(expr.callee))?;
        for &expr in &expr.arguments {
            let arg = self.evaluate(self.ctx.expr.get(expr))?;
            self.arguments_buffer.push(arg)
        }

        if let LoxObjectEnum::Callable(callable) = &*callee {
            let args_len = self.arguments_buffer.len();
            let arity = callable.arity();
            if args_len == arity {
                callable.call(self)
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
        let object = self.evaluate(self.ctx.expr.get(expr.object))?;
        if let LoxObjectEnum::Instance(instance) = &*object {
            instance.get(&expr.name, &mut self.acell_owner)
        } else {
            Err(ControlFlow::abort(
                expr.name.line,
                "Only instances have properties.".to_string(),
            ))
        }
    }

    fn visit_set_expr(&mut self, expr: &SetExpr) -> Self::Output {
        let object = self.evaluate(self.ctx.expr.get(expr.object))?;

        if let LoxObjectEnum::Instance(instance) = &*object {
            let value = self.evaluate(self.ctx.expr.get(expr.value))?;
            instance.set(&expr.name, value.clone(), &mut self.acell_owner);
            Ok(value)
        } else {
            Err(ControlFlow::abort(
                expr.name.line,
                "Only instances have fields.".to_string(),
            ))
        }
    }

    fn visit_this_expr(&mut self, expr: &ThisExpr) -> Self::Output {
        self.lookup_variable(expr, &expr.keyword)
    }

    fn visit_super_expr(&mut self, expr: &SuperExpr) -> Self::Output {
        let loc = expr.get_location().expect("super in class without super");

        let object = self.environment.get_at(loc, &self.acell_owner);
        let superclass = match &*object {
            LoxObjectEnum::Callable(LoxCallable::Class(cls)) => cls,
            _ => panic!("super outside of class"),
        };

        let inst_loc = Location::new(loc.distance - 1, 0);
        let object = self.environment.get_at(inst_loc, &self.acell_owner);
        let instance = match &*object {
            LoxObjectEnum::Instance(inst) => inst,
            _ => panic!("unbound method"),
        };

        let method = superclass.find_method(expr.method.lexeme).ok_or_else(|| {
            ControlFlow::abort(
                expr.method.line,
                format!("Undefined property '{}'", expr.method.lexeme),
            )
        })?;

        Ok(method.bind(instance.clone(), &mut self.acell_owner).into())
    }

    fn visit_fstring_expr(&mut self, expr: &FstringExpr) -> Self::Output {
        use std::fmt::Write;

        let mut output = String::new();
        for &subexpr in &expr.string {
            let _ = write!(
                &mut output,
                "{}",
                self.evaluate(self.ctx.expr.get(subexpr))?
            );
        }

        Ok(LoxObject::string(output))
    }
}

impl StmtVisitor for Interpreter<'_> {
    type Output = Result<(), ControlFlow>;

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output {
        match self.evaluate(self.ctx.expr.get(stmt.expression)) {
            Ok(_) => Ok(()),
            Err(e) => Err(e),
        }
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output {
        let value = self.evaluate(self.ctx.expr.get(stmt.expression))?;
        println!("{}", value);
        Ok(())
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Self::Output {
        let initializer = stmt.initializer.map_or_else(
            || Ok(LoxObject::nil()),
            |expr| self.evaluate(self.ctx.expr.get(expr)),
        )?;

        self.environment.define(initializer, &mut self.acell_owner);
        Ok(())
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::Output {
        self.execute_block(
            &stmt.statements,
            Environment::with_enclosing(self.environment.clone()),
        )
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::Output {
        let condition = self.evaluate(self.ctx.expr.get(stmt.condition))?;
        if condition.is_truthy() {
            self.execute(&stmt.then_branch)?;
        } else if let Some(else_branch) = &stmt.else_branch {
            self.execute(else_branch)?;
        }

        Ok(())
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output {
        while self
            .evaluate(self.ctx.expr.get(stmt.condition))?
            .is_truthy()
        {
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
        let function = LoxObject::function(stmt.clone(), self.environment.clone(), false);
        self.environment.define(function, &mut self.acell_owner);

        Ok(())
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::Output {
        let value = stmt
            .value
            .map(|e| self.evaluate(self.ctx.expr.get(e)))
            .transpose()?;
        Err(ControlFlow::return_(value))
    }

    fn visit_class_stmt(&mut self, stmt: &ClassStmt) -> Self::Output {
        let object;
        let superclass = if let Some(var_expr) = &stmt.superclass {
            object = self.visit_variable_expr(var_expr)?;
            if let LoxObjectEnum::Callable(LoxCallable::Class(superclass)) = &*object {
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

        let index = self
            .environment
            .define(LoxObject::nil(), &mut self.acell_owner);

        let enclosing = if let Some(superclass) = superclass {
            let enclosing = self.environment.clone();
            self.environment = Environment::with_enclosing(enclosing.clone());
            self.environment
                .define(superclass.clone().into(), &mut self.acell_owner);
            Some(enclosing)
        } else {
            None
        };

        let mut methods = FxHashMap::default();
        for method in &stmt.methods {
            let is_initializer = &*method.name.lexeme == "init";
            let function =
                LoxFunction::new(method.clone(), self.environment.clone(), is_initializer);
            methods.insert(method.name.lexeme, function);
        }

        let class = LoxObject::class(stmt.name.lexeme, superclass.cloned(), methods);

        if let Some(enclosing) = enclosing {
            self.environment = enclosing
        }

        let loc = Location::new(0, index);
        self.environment
            .assign_at(loc, class, &mut self.acell_owner);

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

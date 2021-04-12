use crate::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, ExprVisitor, GroupingExpr, LiteralExpr, LogicalExpr,
    UnaryExpr, VariableExpr,
};
use crate::interpreter::Interpreter;
use crate::report_error;
use crate::stmt::{
    BlockStmt, ExprStmt, FunStmt, IfStmt, PrintStmt, ReturnStmt, Stmt, StmtVisitor, VarStmt,
    WhileStmt,
};
use crate::token::Token;
use std::borrow::Cow;
use std::collections::HashMap;

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<HashMap<String, bool>>,
    pub ok: bool,
}

pub trait Resolvable {
    fn resolve(&self, resolver: &mut Resolver<'_>);
}

impl Resolvable for Stmt {
    fn resolve(&self, resolver: &mut Resolver<'_>) {
        self.accept(resolver)
    }
}

impl Resolvable for Expr {
    fn resolve(&self, resolver: &mut Resolver<'_>) {
        self.accept(resolver)
    }
}

impl Resolvable for [Stmt] {
    fn resolve(&self, resolver: &mut Resolver<'_>) {
        for stmt in self {
            stmt.accept(resolver);
        }
    }
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![],
            ok: true,
        }
    }

    fn error(&mut self, line: usize, msg: Cow<'static, str>) {
        report_error("Resolve", line, "", &msg);
        self.ok = false;
    }

    pub fn resolve<T: Resolvable + ?Sized>(&mut self, resolvable: &T) {
        resolvable.resolve(self)
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default())
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn declare(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                self.error(
                    name.line,
                    "Already variable with this name in this scope.".into(),
                );
            } else {
                scope.insert(name.lexeme.clone(), false);
            }
        }
    }

    fn define(&mut self, name: &Token) {
        if let Some(b) = self.scopes.last_mut().and_then(|m| m.get_mut(&name.lexeme)) {
            *b = true;
        }
    }

    fn resolve_local(&mut self, id: usize, name: &Token) {
        for (distance, map) in self.scopes.iter().rev().enumerate() {
            if map.get(&name.lexeme).is_some() {
                self.interpreter.resolve(id, distance);
                return;
            }
        }
    }

    fn resolve_function(&mut self, function: &FunStmt) {
        self.begin_scope();
        for param in &function.params {
            self.declare(param);
            self.define(param);
        }
        let res = self.resolve(function.body.as_slice());
        self.end_scope();

        res
    }
}

impl ExprVisitor for Resolver<'_> {
    type Output = ();

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        self.resolve(&expr.left);
        self.resolve(&expr.right)
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output {
        self.resolve(&expr.expression)
    }

    fn visit_literal_expr(&mut self, _expr: &LiteralExpr) -> Self::Output {}

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output {
        self.resolve(&expr.right)
    }

    fn visit_variable_expr(&mut self, expr: &VariableExpr) -> Self::Output {
        if self
            .scopes
            .last_mut()
            .and_then(|m| m.get(&expr.name.lexeme))
            == Some(&false)
        {
            self.error(
                expr.name.line,
                "Can't read local variable in its own initializer.".into(),
            )
        }

        self.resolve_local(expr.id, &expr.name);
    }

    fn visit_assign_expr(&mut self, expr: &AssignExpr) -> Self::Output {
        self.resolve(&expr.value);
        self.resolve_local(expr.id, &expr.name);
    }

    fn visit_logical_expr(&mut self, expr: &LogicalExpr) -> Self::Output {
        self.resolve(&expr.left);
        self.resolve(&expr.right)
    }

    fn visit_call_expr(&mut self, expr: &CallExpr) -> Self::Output {
        self.resolve(&expr.callee);

        for arg in &expr.arguments {
            self.resolve(arg);
        }
    }
}

impl StmtVisitor for Resolver<'_> {
    type Output = ();

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output {
        self.resolve(&stmt.expression)
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output {
        self.resolve(&stmt.expression)
    }

    fn visit_var_stmt(&mut self, stmt: &VarStmt) -> Self::Output {
        self.declare(&stmt.name);
        if let Some(initializer) = &stmt.initializer {
            self.resolve(initializer);
        }
        self.define(&stmt.name);
    }

    fn visit_block_stmt(&mut self, stmt: &BlockStmt) -> Self::Output {
        self.begin_scope();
        let res = self.resolve(stmt.statements.as_slice());
        self.end_scope();

        res
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::Output {
        self.resolve(&stmt.condition);
        self.resolve(&stmt.then_branch);
        if let Some(else_branch) = &stmt.else_branch {
            self.resolve(else_branch);
        }
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output {
        self.resolve(&stmt.condition);
        self.resolve(&stmt.body)
    }

    fn visit_break_stmt(&mut self) -> Self::Output {}

    fn visit_function_stmt(&mut self, stmt: &FunStmt) -> Self::Output {
        self.declare(&stmt.name);
        self.define(&stmt.name);

        self.resolve_function(stmt)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::Output {
        if let Some(value) = &stmt.value {
            self.resolve(value);
        }
    }
}

use crate::expr::{
    AssignExpr, BinaryExpr, CallExpr, Expr, ExprVisitor, GetExpr, GroupingExpr, LiteralExpr,
    LogicalExpr, SetExpr, SuperExpr, ThisExpr, UnaryExpr, VariableExpr,
};
use crate::interpreter::Interpreter;
use crate::report_error;
use crate::stmt::{
    BlockStmt, BreakStmt, ClassStmt, ExprStmt, FunStmt, IfStmt, PrintStmt, ReturnStmt, Stmt,
    StmtVisitor, VarStmt, WhileStmt,
};
use crate::token::Token;
use std::borrow::Cow;
use std::collections::HashMap;
use std::mem;

#[derive(Debug, Clone, Copy, PartialEq)]
enum FunctionKind {
    None,
    Function,
    Method,
    Initializer,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum ClassKind {
    None,
    Class,
    Subclass,
}

#[derive(Debug, Clone, Copy, PartialEq)]
enum LoopKind {
    None,
    Loop,
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

impl Resolvable for VariableExpr {
    fn resolve(&self, resolver: &mut Resolver<'_>) {
        resolver.visit_variable_expr(self)
    }
}

impl Resolvable for [Stmt] {
    fn resolve(&self, resolver: &mut Resolver<'_>) {
        for stmt in self {
            stmt.accept(resolver);
        }
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    map: HashMap<String, (bool, usize)>,
    var_cnt: usize,
}

impl Scope {
    pub fn insert(&mut self, key: String, value: bool) {
        let current_var_cnt = self.var_cnt;
        self.map.insert(key, (value, current_var_cnt));
        self.var_cnt += 1;
    }
}

#[derive(Debug)]
pub struct Resolver<'a> {
    interpreter: &'a mut Interpreter,
    scopes: Vec<Scope>,
    current_function: FunctionKind,
    current_class: ClassKind,
    current_loop: LoopKind,
    pub had_error: bool,
}

impl<'a> Resolver<'a> {
    pub fn new(interpreter: &'a mut Interpreter) -> Self {
        Self {
            interpreter,
            scopes: vec![Scope::default()],
            current_function: FunctionKind::None,
            current_class: ClassKind::None,
            current_loop: LoopKind::None,
            had_error: false,
        }
    }

    fn error(&mut self, line: usize, msg: Cow<'static, str>) {
        report_error("Resolve", line, "", &msg);
        self.had_error = true;
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
            if scope.map.contains_key(&name.lexeme) {
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
        if let Some((b, _)) = self
            .scopes
            .last_mut()
            .and_then(|s| s.map.get_mut(&name.lexeme))
        {
            *b = true;
        }
    }

    fn resolve_local(&mut self, id: usize, name: &Token) {
        for (distance, scope) in self.scopes.iter().rev().enumerate() {
            if let Some(&(_, index)) = scope.map.get(&name.lexeme) {
                self.interpreter.resolve(id, distance, index);
                return;
            }
        }
    }

    fn resolve_function(&mut self, function: &FunStmt, kind: FunctionKind) {
        let enclosing_function = mem::replace(&mut self.current_function, kind);
        self.begin_scope();

        for param in &function.params {
            self.declare(param);
            self.define(param);
        }
        self.resolve(function.body.as_slice());

        self.end_scope();
        self.current_function = enclosing_function;
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
        if matches!(
            self.scopes
                .last_mut()
                .and_then(|s| s.map.get(&expr.name.lexeme)),
            Some(&(false, _))
        ) {
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

    fn visit_get_expr(&mut self, expr: &GetExpr) -> Self::Output {
        self.resolve(&expr.object);
    }

    fn visit_set_expr(&mut self, expr: &SetExpr) -> Self::Output {
        self.resolve(&expr.value);
        self.resolve(&expr.object);
    }

    fn visit_this_expr(&mut self, expr: &ThisExpr) -> Self::Output {
        if self.current_class == ClassKind::Class {
            self.resolve_local(expr.id, &expr.keyword)
        } else {
            self.error(
                expr.keyword.line,
                "Can't use 'this' outside of a class.".into(),
            )
        }
    }

    fn visit_super_expr(&mut self, expr: &SuperExpr) -> Self::Output {
        match self.current_class {
            ClassKind::None => self.error(
                expr.keyword.line,
                "Can't use 'super' outside of a class.".into(),
            ),
            ClassKind::Class => self.error(
                expr.keyword.line,
                "Can't use 'super' in a class with no superclass.".into(),
            ),
            ClassKind::Subclass => self.resolve_local(expr.id, &expr.keyword),
        }
    }
}

impl StmtVisitor for Resolver<'_> {
    type Output = ();

    fn visit_expr_stmt(&mut self, stmt: &ExprStmt) -> Self::Output {
        self.resolve(&stmt.expression);
    }

    fn visit_print_stmt(&mut self, stmt: &PrintStmt) -> Self::Output {
        self.resolve(&stmt.expression);
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
        self.resolve(stmt.statements.as_slice());
        self.end_scope();
    }

    fn visit_if_stmt(&mut self, stmt: &IfStmt) -> Self::Output {
        self.resolve(&stmt.condition);
        self.resolve(&stmt.then_branch);
        if let Some(else_branch) = &stmt.else_branch {
            self.resolve(else_branch);
        }
    }

    fn visit_while_stmt(&mut self, stmt: &WhileStmt) -> Self::Output {
        let current_loop = mem::replace(&mut self.current_loop, LoopKind::Loop);

        self.resolve(&stmt.condition);
        self.resolve(&stmt.body);

        self.current_loop = current_loop;
    }

    fn visit_break_stmt(&mut self, stmt: &BreakStmt) -> Self::Output {
        if self.current_loop == LoopKind::None {
            self.error(
                stmt.keyword.line,
                "Can't use 'break' outside of a loop".into(),
            )
        }
    }

    fn visit_function_stmt(&mut self, stmt: &FunStmt) -> Self::Output {
        self.declare(&stmt.name);
        self.define(&stmt.name);

        self.resolve_function(stmt, FunctionKind::Function)
    }

    fn visit_return_stmt(&mut self, stmt: &ReturnStmt) -> Self::Output {
        if self.current_function == FunctionKind::None {
            self.error(
                stmt.keyword.line,
                "Can't return from top-level code.".into(),
            )
        }
        if let Some(value) = &stmt.value {
            if self.current_function == FunctionKind::Initializer {
                self.error(
                    stmt.keyword.line,
                    "Can't return a value from an initializer.".into(),
                )
            }
            self.resolve(value);
        }
    }

    fn visit_class_stmt(&mut self, stmt: &ClassStmt) -> Self::Output {
        let enclosing_class = mem::replace(&mut self.current_class, ClassKind::Class);

        // Class name
        self.declare(&stmt.name);
        self.define(&stmt.name);

        // Class superclass
        if let Some(expr) = &stmt.superclass {
            self.current_class = ClassKind::Subclass;
            if stmt.name.lexeme == expr.name.lexeme {
                self.error(expr.name.line, "A class can't inherit from itself.".into())
            }
            self.resolve(expr);
        }

        // Scope for 'super'
        if stmt.superclass.is_some() {
            self.begin_scope();

            self.scopes
                .last_mut()
                .expect("Empty scopes")
                .insert("super".into(), true)
        }

        // Scope for class methods
        self.begin_scope();

        self.scopes
            .last_mut()
            .expect("Empty scopes")
            .insert("this".into(), true);

        for method in &stmt.methods {
            let declaration = if method.name.lexeme == "init" {
                FunctionKind::Initializer
            } else {
                FunctionKind::Method
            };
            self.resolve_function(method, declaration);
        }

        self.end_scope();

        if stmt.superclass.is_some() {
            self.end_scope();
        }

        self.current_class = enclosing_class;
    }
}

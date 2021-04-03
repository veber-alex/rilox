use crate::expr::{BinaryExpr, Expr, ExprVisitor, GroupingExpr, LiteralExpr, UnaryExpr};
pub struct AstPrinter;

impl AstPrinter {
    pub fn print(&mut self, expr: &Expr) -> String {
        expr.accept(self)
    }

    fn parenthesize(&mut self, name: &str, exprs: &[&Expr]) -> String {
        let mut output = format!("({}", name);
        for expr in exprs {
            output.push(' ');
            output.push_str(&expr.accept(self));
        }
        output.push(')');

        output
    }
}

impl ExprVisitor for AstPrinter {
    type Output = String;

    fn visit_binary_expr(&mut self, expr: &BinaryExpr) -> Self::Output {
        self.parenthesize(&expr.operator.lexeme(), &[&expr.left, &expr.right])
    }

    fn visit_grouping_expr(&mut self, expr: &GroupingExpr) -> Self::Output {
        self.parenthesize("group", &[&expr.expression])
    }

    fn visit_literal_expr(&mut self, expr: &LiteralExpr) -> Self::Output {
        expr.value.to_string()
    }

    fn visit_unary_expr(&mut self, expr: &UnaryExpr) -> Self::Output {
        self.parenthesize(&expr.operator.lexeme(), &[&expr.right])
    }

    fn visit_variable_expr(&mut self, expr: &crate::expr::VariableExpr) -> Self::Output {
        todo!()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::object::LoxNumber;
    use crate::token::{Token, TokenType};

    #[test]
    fn pretty_print() {
        let expression = Expr::binary(
            Expr::unary(
                Token::new(TokenType::Minus, "-".into(), 1),
                Expr::literal(LoxNumber::new(123.)),
            ),
            Token::new(TokenType::Star, "*".into(), 1),
            Expr::grouping(Expr::literal(LoxNumber::new(45.2))),
        );

        assert_eq!(AstPrinter.print(&expression), "(* (- 123) (group 45.2))");
    }
}

use std::rc::Rc;

use crate::ast;
use crate::evaluator::Evaluator;
use crate::evaluator::object;

impl Evaluator {

    pub fn quote_and_eval_inner_unquote(&mut self, stmt: &mut ast::Stmt) -> () {
        ast::modify(stmt, |expr| {
            self.unquote_modifier(expr);
        });
    }

    pub fn unquote_modifier(&mut self, expr: &mut ast::Expr) {

        match expr {
            ast::Expr::Call { func, args } => {

                if let ast::Expr::Ident(ident_name) = func.as_ref() {
                    if ident_name.0 == "unquote" {
                        let unquote_expr = args.clone().to_vec()[0].to_owned();
                        let unquote = self.eval(&vec![ast::Stmt::Expr(unquote_expr)]);
                        match unquote {
                            Some(object::Object::Int(x)) => *expr = ast::Expr::Literal(ast::Literal::Int(x)),
                            Some(object::Object::Bool(bool)) => *expr = ast::Expr::Literal(ast::Literal::Bool(bool)),
                            Some(object::Object::Quote(ast::Stmt::Expr(in_quote_expr))) => {
                                *expr = in_quote_expr
                            },
                            Some(object::Object::Null) => {
                                *expr = ast::Expr::Literal(ast::Literal::Int(-1));
                            },
                            _ => {
                                println!("debug what!!={:?}", expr);
                            }
                        }
                    }
                }
            },
            ast::Expr::Infix(infix, left_expr, right_expr) => {
                self.unquote_modifier(left_expr);
                self.unquote_modifier(right_expr);
            },
            ast::Expr::Prefix(prefix, expr) => {
                self.unquote_modifier(expr);
            },
            ast::Expr::If {cond, consequence, alternative } => {
                self.unquote_modifier(cond);
                consequence.iter_mut().for_each(|stmt| { self.quote_and_eval_inner_unquote(stmt) });
                if let Some(alternative) = alternative {
                    alternative.iter_mut().for_each(|stmt| { self.quote_and_eval_inner_unquote(stmt) });
                }
            },
            _ => ()
        }
    }

    // add macro to env and filter out the macro define statement
    pub fn define_macros(&mut self, program: &mut ast::Program) -> () {
        let mut definition_indexes: Vec<usize> = vec![];
        for (i, stmt) in program.iter_mut().enumerate() {
            if let ast::Stmt::Let(ident, ref mut expr) = stmt {
                if let ast::Expr::Macro { params, body } = expr {
                    self.env.borrow_mut().set(ident.0.clone(), object::Object::Macro(params.to_owned(), body.to_owned(), Rc::clone(&self.env)));
                    definition_indexes.push(i);
                }
            }
        }
        *program = program.iter().enumerate().filter(|&(i, _)| { !definition_indexes.contains(&i) }).map(|(i, stmt)| { stmt.clone() }).collect();
    }

    pub fn expand_macros(&mut self, program: &mut ast::Program) -> () {
        for stmt in program.iter_mut() {

            ast::modify(stmt, |expr| {

                self.modifer(expr);
            });
        }
    }

    pub fn modifer(&mut self, expr: &mut ast::Expr) {
        match expr {
            ast::Expr::Call { func, args } => {
                let mut quoted_args: Vec<ast::Expr> = vec![];
                for arg in args.iter() {
                    let quoted_arg = ast::Expr::Call { func: Box::new(ast::Expr::Ident(ast::Ident("quote".to_owned()))), args: vec![arg.to_owned()] };
                    quoted_args.push(quoted_arg);
                }
                let right_evaluated = self.eval(&vec![ast::Stmt::Expr(ast::Expr::Call { func: func.to_owned(), args: quoted_args.to_owned() })]);
                if let Some(object::Object::Quote(ast::Stmt::Expr(right_expr))) = right_evaluated {
                    *expr = right_expr;
                }
            },
            ast::Expr::Infix(infix, left_expr, right_expr) => {
                self.modifer(left_expr);
                self.modifer(right_expr);
            },
            _ => {
                // todo!()
            }
        }
    }
}




#[cfg(test)]
mod tests {
    use super::Evaluator;
    use crate::evaluator::env;
    use crate::evaluator::builtins::new_builtins;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use crate::formatter::Formatter;
    use std::rc::Rc;
    use std::cell::RefCell;

    fn expand_macros_and_format(input: &str) -> String {
        let mut program = Parser::new(Lexer::new(input)).parse();
        let mut evaluator = Evaluator {
            env: Rc::new(RefCell::new(env::Env::from(new_builtins()))),
        };
        let mut formatter = Formatter::default();
        evaluator.define_macros(&mut program);
        evaluator.expand_macros(&mut program);
        formatter.format(program.to_owned())
    }

    #[test]
    fn test_macro_expand_quote() {
        let input = r#"
        let infixExpression = macro() { quote(1 + 2); };

        infixExpression();
        "#;

        assert_eq!(
            "1 + 2;",
            expand_macros_and_format(input)
        );
    }

    #[test]
    fn test_macro_expand_unquote() {
        let input = r#"
        let reverse = macro(a, b) { quote(unquote(b) - unquote(a)); };

        reverse(2 + 2, 10 - 5);
        "#;

        assert_eq!(
            "10 - 5 - 2 + 2;",
            expand_macros_and_format(input)
        );
    }

    #[test]
    fn test_macro_expand_expr_if() {
        let input = r#"
let unless = macro(condition, consequence, alternative) {
    quote(if (!(unquote(condition))) {
        unquote(consequence);
    } else {
        unquote(alternative);
    });
};
unless(10 > 5, puts("not greater"), puts("greater"));
        "#;


        assert_eq!(
            "if (!(10 > 5)) {\n  puts(\"not greater\");\n} else {\n  puts(\"greater\");\n}",
            expand_macros_and_format(input)
        );
    }
}

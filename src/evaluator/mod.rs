use crate::ast;
pub mod env;
pub mod object;

#[derive(PartialEq, Clone, Debug)]
pub struct Evaluator {
    pub env: env::Env
}

impl Evaluator {
    pub fn eval(&mut self, program: &ast::Program) -> Option<object::Object> {
        let mut result = None;
        
        for stmt in program {
            match self.eval_stmt(stmt) {
                Some(object::Object::ReturnValue(value)) => return Some(*value),
                obj => { println!("env {:?} obj {:?}", self.env, obj); result = obj }
            }
        }
        result
    }

    fn eval_stmt(&mut self, stmt: &ast::Stmt) -> Option<object::Object> {
        match stmt {
            ast::Stmt::Let(ident, expr) => {
                let value = match self.eval_expr(expr) {
                    Some(value) => value,
                    None => return None,
                };
                let ast::Ident(name) = ident;
                self.env.set(name.clone(), value);
                None
            },
            ast::Stmt::Break => {
                Some(object::Object::BreakStatement)
            },
            ast::Stmt::Continue => {
                None
            },
            ast::Stmt::Return(expr) => {
                let value = match self.eval_expr(expr) {
                    Some(value) => value,
                    None => return None,
                };
                Some(object::Object::ReturnValue(Box::new(value)))
            }
            ast::Stmt::Expr(expr) => self.eval_expr(expr),
            ast::Stmt::ReAssign(ident, expr) => {
                let value = match self.eval_expr(expr) {
                    Some(value) => value,
                    None => return None,
                };
                let ast::Ident(name) = ident;
                self.env.set(name.clone(), value);
                None
            }
            _ => todo!()
        }
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> Option<object::Object> {
        match expr {
            ast::Expr::Ident(ident) => Some(self.eval_ident(ident)),
            ast::Expr::Literal(literal) => Some(self.eval_literal(literal)),
            ast::Expr::Prefix(prefix, right_expr) => {
                self.eval_expr(&*right_expr).map(|right| self.eval_prefix_expr(prefix, right))
            }
            ast::Expr::Infix(infix, left_expr, right_expr) => {
                let left = self.eval_expr(&*left_expr);
                let right = self.eval_expr(&*right_expr);
                if left.is_some() && right.is_some() {
                    Some(self.eval_infix_expr(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            }
            ast::Expr::While { cond, consequence } => self.eval_while_expr(&*cond, consequence),
            ast::Expr::If {
                cond,
                consequence,
                alternative,
            } => self.eval_if_expr(&*cond, consequence, alternative),
            _ => { None }
        }
    }

    fn eval_ident(&mut self, ident: &ast::Ident) -> object::Object {
        let ast::Ident(name) = ident;

        match self.env.get(name.clone()) {
            Some(value) => value,
            None => panic!(),
        }
    }

    fn eval_prefix_expr(&mut self, prefix: &ast::Prefix, right: object::Object) -> object::Object {
        match prefix {
            ast::Prefix::Not => self.eval_not_op_expr(right),
            ast::Prefix::Minus => self.eval_minus_prefix_op_expr(right),
            ast::Prefix::Plus => self.eval_plus_prefix_op_expr(right),
        }
    }

    fn eval_not_op_expr(&mut self, right: object::Object) -> object::Object {
        todo!();
        // match right {
        //     object::Object::Bool(true) => object::Object::Bool(false),
        //     object::Object::Bool(false) => object::Object::Bool(true),
        //     object::Object::Null => object::Object::Bool(true),
        //     _ => object::Object::Bool(false),
        // }
    }

    fn eval_minus_prefix_op_expr(&mut self, right: object::Object) -> object::Object {
        match right {
            object::Object::Int(value) => object::Object::Int(-value),
            _ => Self::error(format!("unknown operator: -{}", right)),
        }
    }

    fn eval_plus_prefix_op_expr(&mut self, right: object::Object) -> object::Object {
        match right {
            object::Object::Int(value) => object::Object::Int(value),
            _ => Self::error(format!("unknown operator: {}", right)),
        }
    }

    fn eval_infix_expr(&mut self, infix: &ast::Infix, left: object::Object, right: object::Object) -> object::Object {
        match left {
            object::Object::Int(left_value) => {
                if let object::Object::Int(right_value) = right {
                    self.eval_infix_int_expr(infix, left_value, right_value)
                } else {
                    Self::error(format!("type mismatch: {} {} {}", left, infix, right))
                }
            }
            // object::Object::String(left_value) => {
            //     if let object::Object::String(right_value) = right {
            //         self.eval_infix_string_expr(infix, left_value, right_value)
            //     } else {
            //         Self::error(format!("type mismatch: {} {} {}", left_value, infix, right))
            //     }
            // }
            _ => Self::error(format!("unknown operator: {} {} {}", left, infix, right)),
        }
    }


    fn eval_infix_int_expr(&mut self, infix: &ast::Infix, left: i64, right: i64) -> object::Object {
        match infix {
            ast::Infix::Plus => object::Object::Int(left + right),
            ast::Infix::Minus => object::Object::Int(left - right),
            ast::Infix::Multiply => object::Object::Int(left * right),
            ast::Infix::Divide => object::Object::Int(left / right),
            // ast::Infix::LessThan => object::Object::Bool(left < right),
            // ast::Infix::LessThanEqual => object::Object::Bool(left <= right),
            // ast::Infix::GreaterThan => object::Object::Bool(left > right),
            // ast::Infix::GreaterThanEqual => object::Object::Bool(left >= right),
            // ast::Infix::Equal => object::Object::Bool(left == right),
            // ast::Infix::NotEqual => object::Object::Bool(left != right),
            _ => todo!()
        }
    }


    fn eval_literal(&mut self, literal: &ast::Literal) -> object::Object {
        match literal {
            ast::Literal::Int(value) => object::Object::Int(*value),
            _ => panic!()
        }
    }

    fn eval_while_expr(&mut self, cond: &ast::Expr, consequence: &ast::BlockStmt) -> Option<object::Object> {
        let mut result: Option<object::Object> = None;
        loop {
            let cond_result = match self.eval_expr(cond) {
                Some(cond) => cond,
                None => break,
            };
            if !Self::is_truthy(cond_result.clone()) {
                break;
            }
            match self.eval_block_stmt_with_continue_and_break_statement(consequence) {
                Some(object::Object::BreakStatement) => {
                    result = Some(object::Object::Null);
                    break;
                },
                Some(object::Object::ContinueStatement) => {
                    result = Some(object::Object::Null);
                    continue;
                },
                Some(object::Object::ReturnValue(value)) => return Some(object::Object::ReturnValue(value)),
                _ => {}
            }
        }
        result
    }

    fn eval_if_expr(
        &mut self,
        cond: &ast::Expr,
        consequence: &ast::BlockStmt,
        alternative: &Option<ast::BlockStmt>,
    ) -> Option<object::Object> {
        let cond = match self.eval_expr(cond) {
            Some(cond) => cond,
            None => return None,
        };

        if Self::is_truthy(cond) {
            self.eval_block_stmt(consequence)
        } else if let Some(alt) = alternative {
            self.eval_block_stmt(alt)
        } else {
            None
        }
    }



    fn eval_block_stmt(&mut self, stmts: &ast::BlockStmt) -> Option<object::Object> {
        let mut result = None;

        for stmt in stmts {
            // if *stmt == ast::Stmt::Blank {
            //     continue;
            // }

            match self.eval_stmt(stmt) {
                Some(object::Object::ReturnValue(value)) => return Some(object::Object::ReturnValue(value)),
                Some(object::Object::Error(msg)) => return Some(object::Object::Error(msg)),
                obj => result = obj,
                _ => todo!()
            }
        }

        result
    }

    fn eval_block_stmt_with_continue_and_break_statement(&mut self, stmts: &ast::BlockStmt) -> Option<object::Object> {
        let mut result = None;

        for stmt in stmts {
            // if *stmt == ast::Stmt::Blank {
            //     continue;
            // }

            match self.eval_stmt(stmt) {
                Some(object::Object::ReturnValue(value)) => return Some(object::Object::ReturnValue(value)),
                Some(object::Object::BreakStatement) => return Some(object::Object::BreakStatement),
                Some(object::Object::ContinueStatement) => return Some(object::Object::ContinueStatement),
                Some(object::Object::Error(msg)) => return Some(object::Object::Error(msg)),
                obj => result = obj,
                _ => todo!()
            }
        }

        result
    }

    fn error(msg: String) -> object::Object {
        object::Object::Error(msg)
    }

    fn is_truthy(obj: object::Object) -> bool {
        match obj {
            object::Object::Int(0) => false,
            // todo
            _ => true,
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::Evaluator;
    use super::env;
    use super::object;

    fn eval(input: &str) -> Option<object::Object> {
        Evaluator { env: env::Env::new() }
            .eval(&Parser::new(Lexer::new(input)).parse())
    }

    /// cases in edition 2015

    #[test]
    fn test_int_expr() {
        let tests = vec![
            ("5", Some(object::Object::Int(5))),
            ("10", Some(object::Object::Int(10))),
            ("-5", Some(object::Object::Int(-5))),
            ("-10", Some(object::Object::Int(-10))),
            ("+5", Some(object::Object::Int(5))),
            ("+10", Some(object::Object::Int(10))),
            ("+(-5)", Some(object::Object::Int(-5))),
            ("+(-10)", Some(object::Object::Int(-10))),
            ("5 + 5 + 5 + 5 - 10", Some(object::Object::Int(10))),
            ("2 * 2 * 2 * 2 * 2", Some(object::Object::Int(32))),
            ("-50 + 100 + -50", Some(object::Object::Int(0))),
            ("5 * 2 + 10", Some(object::Object::Int(20))),
            ("5 + 2 * 10", Some(object::Object::Int(25))),
            ("20 + 2 * -10", Some(object::Object::Int(0))),
            ("50 / 2 * 2 + 10", Some(object::Object::Int(60))),
            ("2 * (5 + 10)", Some(object::Object::Int(30))),
            ("3 * 3 * 3 + 10", Some(object::Object::Int(37))),
            ("3 * (3 * 3) + 10", Some(object::Object::Int(37))),
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", Some(object::Object::Int(50))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    /// self cases

    // #[test]
    // fn test_let_evaluator() {
    //     let mut lexer = Lexer::new(r"let five = 5;");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     let mut evaluator = Evaluator { env: env::Env::new() };
    //     evaluator.eval(&program);
    // }

    // #[test]
    // fn test_return_evaluator() {
    //     let mut lexer = Lexer::new(r"return 3");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     let mut evaluator = Evaluator { env: env::Env::new() };
    //     let rt = evaluator.eval(&program);
    //     match rt {
    //         Some(object::object::object::Object::Int(v)) => {
    //             assert_eq!(v, 3i64);
    //         },
    //         _ => todo!()
    //     }
    // }

    // #[test]
    // fn test_expr_evaluator() {
    //     let mut lexer = Lexer::new(r"6");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     let mut evaluator = Evaluator { env: env::Env::new() };
    //     let rt = evaluator.eval(&program);
    //     match rt {
    //         Some(object::object::object::Object::Int(v)) => {
    //             assert_eq!(v, 6i64);
    //         },
    //         _ => todo!()
    //     }
    // }

    #[test]
    fn test_reassign_evaluator() {
        let mut lexer = Lexer::new(r"let five = 5; five = 6");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator { env: env::Env::new() };
        evaluator.eval(&program);
    }

    #[test]
    fn test_while_evaluator() {
        let mut lexer = Lexer::new(r"let a = 5; while (a) { a = 0; }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator { env: env::Env::new() };
        evaluator.eval(&program);
    }

    #[test]
    fn test_break_evaluator() {
        let mut lexer = Lexer::new(r"let a = 5; while (a) { a = 3; break; }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator { env: env::Env::new() };
        evaluator.eval(&program);
    }

    #[test]
    fn test_if_else_evaluator() {
        let mut lexer = Lexer::new(r"let a = 2; let b = 0;; if (a) { b = 1 } else { b = 2 }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("program {:?}", program);
        let mut evaluator = Evaluator { env: env::Env::new() };
        evaluator.eval(&program);
    }

    #[test]
    fn test_continue_evaluator() {
        let mut lexer = Lexer::new(r"let a = 5; let b = 7; while (a) { if (b) { a = 3; b = 0; continue; } else { a = 0; b = 1; break; } }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("program {:?}", program);
        let mut evaluator = Evaluator { env: env::Env::new() };
        evaluator.eval(&program);
    }
}
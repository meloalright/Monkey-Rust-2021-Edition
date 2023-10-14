use crate::ast;
pub mod env;
pub mod object;

#[derive(PartialEq, Clone, Debug)]
pub struct Evaluator {
    pub env: env::Env,
}

///
// Evaluator Basic Implement
///
impl Evaluator {
    pub fn new(env: env::Env) -> Self {
        Evaluator { env }
    }

    pub fn eval(&mut self, program: &ast::Program) -> Option<object::Object> {
        let mut result = None;

        for stmt in program {
            match self.eval_stmt(stmt) {
                Some(object::Object::ReturnValue(value)) => return Some(*value),
                Some(object::Object::Error(msg)) => return Some(object::Object::Error(msg)),
                obj => result = obj,
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
            }
            ast::Stmt::Break => Some(object::Object::BreakStatement),
            ast::Stmt::Continue => None,
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
            _ => todo!(),
        }
    }
}

///
// truthy + Error Eval Implement
///
impl Evaluator {
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

    fn is_error(obj: &object::Object) -> bool {
        match obj {
            object::Object::Error(_) => true,
            _ => false,
        }
    }
}

///
// Condition Eval Implement
///
impl Evaluator {
    // if
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

    // while
    fn eval_while_expr(
        &mut self,
        cond: &ast::Expr,
        consequence: &ast::BlockStmt,
    ) -> Option<object::Object> {
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
                }
                Some(object::Object::ContinueStatement) => {
                    result = Some(object::Object::Null);
                    continue;
                }
                Some(object::Object::ReturnValue(value)) => {
                    return Some(object::Object::ReturnValue(value))
                }
                _ => {}
            }
        }
        result
    }

    // continue + break
    fn eval_block_stmt_with_continue_and_break_statement(
        &mut self,
        stmts: &ast::BlockStmt,
    ) -> Option<object::Object> {
        let mut result = None;

        for stmt in stmts {
            // if *stmt == ast::Stmt::Blank {
            //     continue;
            // }

            match self.eval_stmt(stmt) {
                Some(object::Object::ReturnValue(value)) => {
                    return Some(object::Object::ReturnValue(value))
                }
                Some(object::Object::BreakStatement) => {
                    return Some(object::Object::BreakStatement)
                }
                Some(object::Object::ContinueStatement) => {
                    return Some(object::Object::ContinueStatement)
                }
                Some(object::Object::Error(msg)) => return Some(object::Object::Error(msg)),
                obj => result = obj,
                _ => todo!(),
            }
        }

        result
    }

    // block
    fn eval_block_stmt(&mut self, stmts: &ast::BlockStmt) -> Option<object::Object> {
        let mut result = None;

        for stmt in stmts {
            // if *stmt == ast::Stmt::Blank {
            //     continue;
            // }

            match self.eval_stmt(stmt) {
                Some(object::Object::ReturnValue(value)) => {
                    return Some(object::Object::ReturnValue(value))
                }
                Some(object::Object::Error(msg)) => return Some(object::Object::Error(msg)),
                obj => result = obj,
                _ => todo!(),
            }
        }

        result
    }
}

/// Expr Eval Implement
impl Evaluator {
    fn eval_expr(&mut self, expr: &ast::Expr) -> Option<object::Object> {
        match expr {
            ast::Expr::Ident(ident) => Some(self.eval_ident(ident)),
            ast::Expr::Literal(literal) => Some(self.eval_literal(literal)),
            ast::Expr::Prefix(prefix, right_expr) => self
                .eval_expr(&*right_expr)
                .map(|right| self.eval_prefix_expr(prefix, right)),
            ast::Expr::Infix(infix, left_expr, right_expr) => {
                let left = self.eval_expr(&*left_expr);
                let right = self.eval_expr(&*right_expr);
                if left.is_some() && right.is_some() {
                    Some(self.eval_infix_expr(infix, left.unwrap(), right.unwrap()))
                } else {
                    None
                }
            }
            ast::Expr::Index(left_expr, index_expr) => {
                let left = self.eval_expr(&*left_expr);
                let index = self.eval_expr(&*index_expr);
                if left.is_some() && index.is_some() {
                    Some(self.eval_index_expr(left.unwrap(), index.unwrap()))
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
            _ => None,
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

    fn eval_infix_expr(
        &mut self,
        infix: &ast::Infix,
        left: object::Object,
        right: object::Object,
    ) -> object::Object {
        match left {
            object::Object::Int(left_value) => {
                if let object::Object::Int(right_value) = right {
                    self.eval_infix_int_expr(infix, left_value, right_value)
                } else {
                    Self::error(format!("type mismatch: {} {} {}", left, infix, right))
                }
            }
            object::Object::String(left_value) => {
                if let object::Object::String(right_value) = right {
                    self.eval_infix_string_expr(infix, left_value, right_value)
                } else {
                    Self::error(format!("type mismatch: {} {} {}", left_value, infix, right))
                }
            }
            _ => Self::error(format!("unknown operator: {} {} {}", left, infix, right)),
        }
    }

    fn eval_infix_int_expr(&mut self, infix: &ast::Infix, left: i64, right: i64) -> object::Object {
        match infix {
            ast::Infix::Plus => object::Object::Int(left + right),
            ast::Infix::Minus => object::Object::Int(left - right),
            ast::Infix::Multiply => object::Object::Int(left * right),
            ast::Infix::Divide => object::Object::Int(left / right),
            ast::Infix::LT => object::Object::Bool(left < right),
            ast::Infix::LTEQ => object::Object::Bool(left <= right),
            ast::Infix::GT => object::Object::Bool(left > right),
            ast::Infix::GTEQ => object::Object::Bool(left >= right),
            ast::Infix::Equal => object::Object::Bool(left == right),
            ast::Infix::NotEqual => object::Object::Bool(left != right),
            _ => todo!(),
        }
    }

    fn eval_infix_string_expr(
        &mut self,
        infix: &ast::Infix,
        left: String,
        right: String,
    ) -> object::Object {
        match infix {
            ast::Infix::Plus => object::Object::String(format!("{}{}", left, right)),
            _ => object::Object::Error(format!("unknown operator: {} {} {}", left, infix, right)),
        }
    }
}

///
// Function Eval Implement
///
impl Evaluator {
    fn eval_call_expr() {
        todo!()
    }
}

///
// ...
///
impl Evaluator {
    fn eval_ident(&mut self, ident: &ast::Ident) -> object::Object {
        let ast::Ident(name) = ident;

        match self.env.get(name.clone()) {
            Some(value) => value,
            None => panic!(),
        }
    }

    fn eval_literal(&mut self, literal: &ast::Literal) -> object::Object {
        match literal {
            ast::Literal::Int(value) => object::Object::Int(*value),
            ast::Literal::String(value) => object::Object::String(value.clone()),
            ast::Literal::Bool(value) => object::Object::Bool(*value),
            ast::Literal::Array(objects) => self.eval_array_literal(objects),
            ast::Literal::Hash(pairs) => self.eval_hash_literal(pairs),
            _ => panic!(),
        }
    }
}

///
// array, hash, index ...
///
impl Evaluator {
    fn eval_array_literal(&mut self, objects: &Vec<ast::Expr>) -> object::Object {
        object::Object::Array(
            objects
                .iter()
                .map(|e| self.eval_expr(&e.clone()).unwrap_or(object::Object::Null))
                .collect::<Vec<_>>(),
        )
    }

    fn eval_index_expr(&mut self, left: object::Object, index: object::Object) -> object::Object {
        match left {
            object::Object::Array(ref array) => {
                if let object::Object::Int(i) = index {
                    self.eval_array_index_expr(array.clone(), i)
                } else {
                    Self::error(format!("index operator not supported: {}", left))
                }
            }
            object::Object::Hash(ref hash) => match index {
                object::Object::Int(_) | object::Object::Bool(_) | object::Object::String(_) => match hash.get(&index) {
                    Some(o) => o.clone(),
                    None => object::Object::Null,
                },
                object::Object::Error(_) => index,
                _ => Self::error(format!("unusable as hash key: {}", index)),
            },
            _ => Self::error(format!("uknown operator: {} {}", left, index)),
        }
    }

    fn eval_array_index_expr(&mut self, array: Vec<object::Object>, index: i64) -> object::Object {
        let max = array.len() as i64;

        if index < 0 || index > max {
            return object::Object::Null;
        }

        match array.get(index as usize) {
            Some(o) => o.clone(),
            None => object::Object::Null,
        }
    }

    fn eval_hash_literal(&mut self, pairs: &Vec<(ast::Expr, ast::Expr)>) -> object::Object {
        let mut hash = std::collections::HashMap::new();

        for (key_expr, value_expr) in pairs {
            let key = self.eval_expr(key_expr).unwrap_or(object::Object::Null);
            if Self::is_error(&key) {
                return key;
            }

            let value = self.eval_expr(value_expr).unwrap_or(object::Object::Null);
            if Self::is_error(&value) {
                return value;
            }

            hash.insert(key, value);
        }

        object::Object::Hash(hash)
    }
}

#[cfg(test)]
mod tests {
    use super::env;
    use super::object;
    use super::Evaluator;
    use crate::lexer::Lexer;
    use crate::parser::Parser;

    fn eval(input: &str) -> Option<object::Object> {
        Evaluator {
            env: env::Env::new(),
        }
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
            (
                "(5 + 10 * 2 + 15 / 3) * 2 + -10",
                Some(object::Object::Int(50)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_string_expr() {
        let input = "\"Hello World!\"";

        assert_eq!(
            Some(object::Object::String(String::from("Hello World!"))),
            eval(input)
        );
    }

    #[test]
    fn test_string_concatenation() {
        let input = "\"Hello\" + \" \" + \"World!\"";

        assert_eq!(
            Some(object::Object::String(String::from("Hello World!"))),
            eval(input)
        );
    }

    #[test]
    fn test_boolean_expr() {
        let tests = vec![
            ("true", Some(object::Object::Bool(true))),
            ("false", Some(object::Object::Bool(false))),
            ("1 < 2", Some(object::Object::Bool(true))),
            ("1 > 2", Some(object::Object::Bool(false))),
            ("1 < 1", Some(object::Object::Bool(false))),
            ("1 > 1", Some(object::Object::Bool(false))),
            ("1 >= 1", Some(object::Object::Bool(true))),
            ("1 <= 1", Some(object::Object::Bool(true))),
            ("1 >= 2", Some(object::Object::Bool(false))),
            ("1 <= 1", Some(object::Object::Bool(true))),
            ("2 <= 1", Some(object::Object::Bool(false))),
            ("1 == 1", Some(object::Object::Bool(true))),
            ("1 != 1", Some(object::Object::Bool(false))),
            ("1 == 2", Some(object::Object::Bool(false))),
            ("1 != 2", Some(object::Object::Bool(true))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_array_literal() {
        let input = "[1, 2 * 2, 3 + 3]";

        assert_eq!(
            Some(object::Object::Array(vec![
                object::Object::Int(1),
                object::Object::Int(4),
                object::Object::Int(6),
            ])),
            eval(input),
        );
    }

    #[test]
    fn test_array_index_expr() {
        let tests = vec![
            ("[1, 2, 3][0]", Some(object::Object::Int(1))),
            ("[1, 2, 3][1]", Some(object::Object::Int(2))),
            ("let i = 0; [1][i]", Some(object::Object::Int(1))),
            ("[1, 2, 3][1 + 1];", Some(object::Object::Int(3))),
            (
                "let myArray = [1, 2, 3]; myArray[2];",
                Some(object::Object::Int(3)),
            ),
            (
                "let myArray = [1, 2, 3]; myArray[0] + myArray[1] + myArray[2];",
                Some(object::Object::Int(6)),
            ),
            (
                "let myArray = [1, 2, 3]; let i = myArray[0]; myArray[i];",
                Some(object::Object::Int(2)),
            ),
            ("[1, 2, 3][3]", Some(object::Object::Null)),
            ("[1, 2, 3][-1]", Some(object::Object::Null)),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_hash_literal() {
        let input = r#"
let two = "two";
{
  "one": 10 - 9,
  two: 1 + 1,
  "thr" + "ee": 6 / 2,
  4: 4,
  true: 5,
  false: 6
}
"#;

        let mut hash = std::collections::HashMap::new();
        hash.insert(object::Object::String(String::from("one")), object::Object::Int(1));
        hash.insert(object::Object::String(String::from("two")), object::Object::Int(2));
        hash.insert(object::Object::String(String::from("three")), object::Object::Int(3));
        hash.insert(object::Object::Int(4), object::Object::Int(4));
        hash.insert(object::Object::Bool(true), object::Object::Int(5));
        hash.insert(object::Object::Bool(false), object::Object::Int(6));

        assert_eq!(Some(object::Object::Hash(hash)), eval(input),);
    }

    #[test]
    fn test_hash_index_expr() {
        let tests = vec![
            ("{\"foo\": 5}[\"foo\"]", Some(object::Object::Int(5))),
            ("{\"foo\": 5}[\"bar\"]", Some(object::Object::Null)),
            ("let key = \"foo\"; {\"foo\": 5}[key]", Some(object::Object::Int(5))),
            ("{}[\"foo\"]", Some(object::Object::Null)),
            ("{5: 5}[5]", Some(object::Object::Int(5))),
            ("{true: 5}[true]", Some(object::Object::Int(5))),
            ("{false: 5}[false]", Some(object::Object::Int(5))),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_not_operator() {}

    #[test]
    fn test_if_else_expr() {}

    #[test]
    fn test_return_stmt() {}

    #[test]
    fn test_let_stmt() {
        let tests = vec![
            ("let a = 5; a;", Some(object::Object::Int(5))),
            ("let a = 5 * 5; a;", Some(object::Object::Int(25))),
            ("let a = 5; let b = a; b;", Some(object::Object::Int(5))),
            (
                "let a = 5; let b = a; let c = a + b + 5; c;",
                Some(object::Object::Int(15)),
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_const_stmt() {}

    #[test]
    fn test_assign_stmt() {
        let tests = vec![("let a = 5; a = 3; a", Some(object::Object::Int(3)))];

        for (input, expect) in tests {
            assert_eq!(expect, eval(input));
        }
    }

    #[test]
    fn test_blank_stmt() {}

    #[test]
    fn test_fn_object() {}

    #[test]
    fn test_fn_application() {}

    #[test]
    fn test_closures() {}

    #[test]
    fn test_builtin_functions() {}

    #[test]
    fn test_error_handling() {
        let tests = vec![
            (
                "5 + true",
                Some(object::Object::Error(String::from(
                    "type mismatch: 5 + true",
                ))),
            ),
            (
                "5 + true; 5;",
                Some(object::Object::Error(String::from(
                    "type mismatch: 5 + true",
                ))),
            ),
            (
                "-true",
                Some(object::Object::Error(String::from(
                    "unknown operator: -true",
                ))),
            ),
            (
                "5; true + false; 5;",
                Some(object::Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "if (10 > 1) { true + false; }",
                Some(object::Object::Error(String::from(
                    "unknown operator: true + false",
                ))),
            ),
            (
                "\"Hello\" - \"World\"",
                Some(object::Object::Error(String::from(
                    "unknown operator: Hello - World",
                ))),
            ),
            // todo cases
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
        let mut evaluator = Evaluator {
            env: env::Env::new(),
        };
        evaluator.eval(&program);
    }

    #[test]
    fn test_while_evaluator() {
        let mut lexer = Lexer::new(r"let a = 5; while (a) { a = 0; }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator {
            env: env::Env::new(),
        };
        evaluator.eval(&program);
    }

    #[test]
    fn test_break_evaluator() {
        let mut lexer = Lexer::new(r"let a = 5; while (a) { a = 3; break; }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator {
            env: env::Env::new(),
        };
        evaluator.eval(&program);
    }

    #[test]
    fn test_if_else_evaluator() {
        let mut lexer = Lexer::new(r"let a = 2; let b = 0;; if (a) { b = 1 } else { b = 2 }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("program {:?}", program);
        let mut evaluator = Evaluator {
            env: env::Env::new(),
        };
        evaluator.eval(&program);
    }

    #[test]
    fn test_continue_evaluator() {
        let mut lexer = Lexer::new(
            r"let a = 5; let b = 7; while (a) { if (b) { a = 3; b = 0; continue; } else { a = 0; b = 1; break; } }",
        );
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("program {:?}", program);
        let mut evaluator = Evaluator {
            env: env::Env::new(),
        };
        evaluator.eval(&program);
    }
}

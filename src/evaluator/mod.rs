use crate::ast;
pub mod env;
pub mod object;

#[derive(PartialEq, Clone, Debug)]
pub struct Evaluator {
    pub env: env::Env
}

impl Evaluator {
    pub fn eval(&mut self, program: &ast::Program) {
        for stmt in program {
            match self.eval_stmt(stmt) {
                _ => println!("env {:?}", self.env)
            }
        }
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
        }
    }

    fn eval_expr(&mut self, expr: &ast::Expr) -> Option<object::Object> {
        match expr {
            ast::Expr::Literal(literal) => Some(self.eval_literal(literal)),
            _ => { None }
        }
    }

    // fn eval_ident(&mut self, ident: &ast::Ident) -> object::Object {
    //     let ast::Ident(name) = ident;

    //     match self.env.get(name.clone()) {
    //         Some(value) => value,
    //         None => panic!(),
    //     }
    // }


    fn eval_literal(&mut self, literal: &ast::Literal) -> object::Object {
        match literal {
            ast::Literal::Int(value) => object::Object::Int(*value),
            _ => panic!()
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use super::Evaluator;
    use super::env;
    // use crate::Token;

    #[test]
    fn test_evaluator() {
        let mut lexer = Lexer::new(r"let five = 5;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        let mut evaluator = Evaluator { env: env::Env::new() };
        evaluator.eval(&program);
    }
}
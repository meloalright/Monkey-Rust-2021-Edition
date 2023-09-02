use crate::ast::*;
use crate::lexer::Lexer;
use std::fmt;
use crate::token::Token;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,
}

impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Eof,
            next_token: Token::Eof,
        };

        parser.bump();
        parser.bump();

        parser
    }

    fn bump(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    pub fn parse(&mut self) -> Program {
        let mut program: Program = vec![];

        while self.current_token != Token::Eof {
            match self.parse_stmt() {
                Some(stmt) => program.push(stmt),
                None => {}
            }
            self.bump();
        }

        program
    }

    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current_token {
            Token::Let => self.parse_let_stmt(),
            _ => panic!(),
        }
    }

    fn parse_let_stmt(&mut self) -> Option<Stmt> {
        match &self.next_token {
            Token::Ident(_) => self.bump(),
            _ => return None,
        };

        let name = match self.parse_ident() {
            Some(name) => name,
            None => return None,
        };

        // if !self.expect_next_token(Token::Assign) {
        //     return None;
        // }
        self.bump();

        if self.current_token == Token::Assign {
            // ok
        }

        self.bump();

        // println!("aaa{:?}", self.current_token);

        let expr = match self.current_token /* ? */ {
            Token::Int(i) => {
                if self.next_token == Token::Semicolon {
                    self.bump();
                }
                Expr::Literal(Literal::Int(i))
            },
            _ => panic!("aaa{:?}", self.current_token)
        };

    
        // let expr = match self.parse_expr(Precedence::Lowest) {
        //     Some(expr) => expr,
        //     None => return None,
        // };

        // if self.next_token_is(&Token::Semicolon) {
        //     self.bump();
        // }

        Some(Stmt::Let(name, expr))
    }


    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token {
            Token::Ident(ref mut ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }


}


#[cfg(test)]
mod tests {
    use crate::ast::Stmt;
    use crate::ast::Expr;
    use crate::ast::Literal;

    use super::Lexer;
    use super::Parser;

    #[test]
    fn test_parser_let_stmt() {

        let mut lexer = Lexer::new(r"let five = 5;");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Let(ident, expr) => {
                        assert_eq!(ident.0, "five");
                        match expr {
                            Expr::Literal(ltr) => {
                                match ltr {
                                    Literal::Int(i) => {
                                        assert_eq!(*i, 5i64)
                                    },
                                    _ => {}
                                }
                            },
                            _ => {}
                        }
                    },
                    _ => {}
                }
            },
            _ => {}
        }
    }
}
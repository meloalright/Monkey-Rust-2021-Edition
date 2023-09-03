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

        parser.walk_token();
        parser.walk_token();

        parser
    }

    fn walk_token(&mut self) {
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
            self.walk_token();
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
            Token::Ident(_) => self.walk_token(),
            _ => return None,
        };

        let name = match self.parse_ident() {
            Some(name) => name,
            None => return None,
        };

        self.walk_token();

        if self.current_token != Token::Assign {
            return None;
        }

        self.walk_token();


        let expr = match self.current_token /* ? */ {
            Token::Int(i) => {
                if self.next_token == Token::Semicolon {
                    self.walk_token();
                }
                Expr::Literal(Literal::Int(i))
            },
            _ => panic!("aaa{:?}", self.current_token)
        };


        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Let(name, expr))
    }


    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token {
            Token::Ident(ref mut ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }

    fn current_token_is(&mut self, tok: Token) -> bool {
        self.current_token == tok
    }

    fn next_token_is(&mut self, tok: Token) -> bool {
        self.next_token == tok
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

    #[test]
    fn test_parser_illegal_let_stmt() {

        let mut lexer = Lexer::new(r"let five 5");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert_eq!(program.len(), 0);
    }
}
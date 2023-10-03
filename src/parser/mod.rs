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
            Token::Const => todo!(),
            Token::Return => self.parse_return_stmt(),
            Token::Ident(_) => match self.next_token {
                Token::Assign => self.parse_reassign_stmt(),
                _ => self.parse_expr_stmt()
            },
            Token::Break => self.parse_break_stmt(),
            Token::Continue => self.parse_continue_stmt(),
            Token::Blank => Some(Stmt::Blank),
            _ => self.parse_expr_stmt(),
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

    fn parse_break_stmt(&mut self) -> Option<Stmt> {
        self.walk_token();

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Break)
    }

    fn parse_continue_stmt(&mut self) -> Option<Stmt> {
        self.walk_token();

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Continue)
    }

    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.walk_token();

        let expr = match self.parse_expr() {
            Some(expr) => expr,
            _ => return None
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Return(expr))
    }

    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = match self.parse_expr() {
            Some(expr) => expr,
            _ => return None
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Expr(expr))
    }

    fn parse_reassign_stmt(&mut self) -> Option<Stmt> {
        let name = match self.parse_ident() {
            Some(name) => name,
            None => return None
        };

        if !self.next_token_is(Token::Assign) {
            return None;
        } else {
            self.walk_token();
        }

        self.walk_token();

        let expr = match self.parse_expr() {
            Some(expr) => expr,
            None => return None
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::ReAssign(name, expr))
    }
    
    fn parse_expr(&mut self) -> Option<Expr> {
        let mut left = match self.current_token {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::Int(_) => self.parse_int_expr(),
            Token::String(_) => todo!(),
            Token::Bool(_) => todo!(),
            Token::LBracket => todo!(),
            Token::LBrace => todo!(),
            Token::LParen => todo!(),
            Token::Bang | Token::Minus | Token::Plus => todo!(),
            Token::If => self.parse_if_expr(),
            Token::While => self.parse_while_expr(),
            Token::Function => todo!(),
            _ => {
                // todo!();
                None
            }
        };

        // infix
        // while !self.next_token_is(Token::Semicolon) /*&& precedence < self.next_token_precedence() */ {
        //     match self.next_token {
        //         Token::Plus
        //         | Token::Minus
        //         | Token::Slash
        //         | Token::Asterisk
        //         | Token::Equal
        //         | Token::NotEqual
        //         /* | Token::LessThan
        //         | Token::LessThanEqual
        //         | Token::GreaterThan
        //         | Token::GreaterThanEqual */ => {
        //             // self.walk_token();
        //             todo!()
        //             // left = self.parse_infix_expr(left.unwrap());
        //         }
        //         Token::LBracket => {
        //             self.walk_token();
        //             todo!()
        //             // left = self.parse_index_expr(left.unwrap());
        //         }
        //         Token::Dot => {
        //             self.walk_token();
        //             todo!()
        //             // left = self.parse_dot_index_expr(left.unwrap());
        //         }
        //         Token::LParen => {
        //             self.walk_token();
        //             todo!()
        //             // left = self.parse_call_expr(left.unwrap());
        //         }
        //         _ => return left,
        //     }
        // }
        // // todo

        left
    }


    // parse expr ...

    fn parse_ident_expr(&mut self) -> Option<Expr> {
        self.parse_ident().map(Expr::Ident)
    }

    fn parse_int_expr(&mut self) -> Option<Expr> {
        match self.current_token {
            Token::Int(ref mut int) => Some(Expr::Literal(Literal::Int(*int))),
            _ => None,
        }
    }

    fn parse_if_expr(&mut self) -> Option<Expr> {
        if !self.next_token_is(Token::LParen) {
            return None;
        }

        self.walk_token();
        self.walk_token();

        let cond = match self.parse_expr() {
            Some(expr) => expr,
            None => return None,
        };

        if !self.next_token_is(Token::RParen) {
            return None;
        }
        self.walk_token();

        if !self.next_token_is(Token::LBrace) {
            return None;
        }
        self.walk_token();

        let consequence = self.parse_block_stmt();

        let mut alternative = None;

        if self.next_token_is(Token::Else) {
            self.walk_token();
            if !self.next_token_is(Token::LBrace) {
                self.walk_token();
                return None;
            }
            self.walk_token();

            alternative = Some(self.parse_block_stmt());
        }

        Some(Expr::If { cond: Box::new(cond), consequence, alternative })

    }

    fn parse_while_expr(&mut self) -> Option<Expr> {
        if !self.next_token_is(Token::LParen) {
            return None;
        }

        self.walk_token();
        self.walk_token();

        let cond = match self.parse_expr() {
            Some(expr) => expr,
            None => return None,
        };

        if !self.next_token_is(Token::RParen) {
            return None;
        }
        self.walk_token();

        if !self.next_token_is(Token::LBrace) {
            return None;
        }
        self.walk_token();

        let consequence = self.parse_block_stmt();

        Some(Expr::While {
            cond: Box::new(cond),
            consequence,
        })
    }

    
    fn parse_block_stmt(&mut self) -> BlockStmt {
        self.walk_token();

        let mut block: Vec<Stmt> = vec![];

        while !self.current_token_is(Token::RBrace) {
            
            match self.parse_stmt() {
                Some(stmt) => block.push(stmt),
                None => {}
            }
            self.walk_token();
        }

        block
    }


    // 

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
                                    _ => todo!()
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }

    #[test]
    fn test_parser_return_stmt() {

        let mut lexer = Lexer::new(r"return a");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Return(expr) => {
                        match expr {
                            Expr::Ident(ltr) => {
                                assert_eq!(ltr.0, "a")
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }

    #[test]
    fn test_parser_expr_stmt() {

        let mut lexer = Lexer::new(r"i");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr {
                            Expr::Ident(ltr) => {
                                assert_eq!(ltr.0, "i")
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }

    #[test]
    fn test_parser_reassign_stmt() {

        let mut lexer = Lexer::new(r"i = 3");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::ReAssign(name, expr) => {
                        assert_eq!(name.0, "i");
                        match expr {
                            Expr::Literal(ltr) => {
                                match ltr {
                                    Literal::Int(i) => {
                                        assert_eq!(*i, 3i64)
                                    },
                                    _ => todo!()
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }

    #[test]
    fn test_parser_while_stmt() {

        let mut lexer = Lexer::new(r"while (w) { q = 5 }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("{:?}", program);
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr {
                            Expr::While { cond, consequence } => {
                                match *cond.clone() {
                                    Expr::Ident(ident) => {
                                        assert_eq!(ident.0, "w")
                                    },
                                    _ => todo!()
                                }
                                assert_eq!(consequence.len(), 1);
                                match consequence.get(0) {
                                    Some(stmt) => {
                                        match stmt {
                                            Stmt::ReAssign(ident, expr) => {
                                                assert_eq!(ident.0, "q");
                                                match expr {
                                                    Expr::Literal(ltr) => {
                                                        match ltr {
                                                            Literal::Int(i) => {
                                                                assert_eq!(*i, 5i64)
                                                            },
                                                            _ => todo!()
                                                        }
                                                    },
                                                    _ => todo!()
                                                }
                                            },
                                            _ => todo!()
                                        }
                                    },
                                    _ => todo!()
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }


    #[test]
    fn test_parser_break_stmt() {

        let mut lexer = Lexer::new(r"while (w) { break; }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("{:?}", program);
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr {
                            Expr::While { cond, consequence } => {
                                match *cond.clone() {
                                    Expr::Ident(ident) => {
                                        assert_eq!(ident.0, "w")
                                    },
                                    _ => todo!()
                                }
                                assert_eq!(consequence.len(), 1);
                                match consequence.get(0) {
                                    Some(stmt) => {
                                        assert_eq!(stmt, &Stmt::Break);
                                    },
                                    _ => todo!()
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }


    #[test]
    fn test_parser_continue_stmt() {

        let mut lexer = Lexer::new(r"while (w) { continue; }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("{:?}", program);
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr {
                            Expr::While { cond, consequence } => {
                                match *cond.clone() {
                                    Expr::Ident(ident) => {
                                        assert_eq!(ident.0, "w")
                                    },
                                    _ => todo!()
                                }
                                assert_eq!(consequence.len(), 1);
                                match consequence.get(0) {
                                    Some(stmt) => {
                                        assert_eq!(stmt, &Stmt::Continue);
                                    },
                                    _ => todo!()
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }

    #[test]
    fn test_parser_illegal_let_stmt() {

        let mut lexer = Lexer::new(r"let five 5");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        assert_eq!(program.len(), 0);
    }


    #[test]
    fn test_parser_if_stmt() {

        let mut lexer = Lexer::new(r"if (w) { q = 5 } else { q = 1 }");
        let mut parser = Parser::new(lexer);
        let program = parser.parse();
        println!("{:?}", program);
        assert_eq!(program.len(), 1);
        match program.get(0) {
            Some(stmt) => {
                match stmt {
                    Stmt::Expr(expr) => {
                        match expr {
                            Expr::If { cond, consequence, alternative } => {
                                match *cond.clone() {
                                    Expr::Ident(ident) => {
                                        assert_eq!(ident.0, "w")
                                    },
                                    _ => todo!()
                                }
                                assert_eq!(consequence.len(), 1);
                                match consequence.get(0) {
                                    Some(stmt) => {
                                        match stmt {
                                            Stmt::ReAssign(ident, expr) => {
                                                assert_eq!(ident.0, "q");
                                                match expr {
                                                    Expr::Literal(ltr) => {
                                                        match ltr {
                                                            Literal::Int(i) => {
                                                                assert_eq!(*i, 5i64)
                                                            },
                                                            _ => todo!()
                                                        }
                                                    },
                                                    _ => todo!()
                                                }
                                            },
                                            _ => todo!()
                                        }
                                    },
                                    _ => todo!()
                                }
                                if let Some(alternative) = alternative {
                                    assert_eq!(alternative.len(), 1);
                                    match alternative.get(0) {
                                        Some(stmt) => {
                                            match stmt {
                                                Stmt::ReAssign(ident, expr) => {
                                                    assert_eq!(ident.0, "q");
                                                    match expr {
                                                        Expr::Literal(ltr) => {
                                                            match ltr {
                                                                Literal::Int(i) => {
                                                                    assert_eq!(*i, 1i64)
                                                                },
                                                                _ => todo!()
                                                            }
                                                        },
                                                        _ => todo!()
                                                    }
                                                },
                                                _ => todo!()
                                            }
                                        },
                                        _ => todo!()
                                    }
                                }
                            },
                            _ => todo!()
                        }
                    },
                    _ => todo!()
                }
            },
            _ => todo!()
        }
    }


}
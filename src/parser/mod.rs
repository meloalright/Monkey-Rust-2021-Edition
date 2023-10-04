use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

pub struct Parser {
    lexer: Lexer,
    current_token: Token,
    next_token: Token,
    errors: ParseErrors,
}

#[derive(Debug, Clone)]
pub enum ParseError {
    UnexpectedToken { want: Option<Token>, got: Token },
}

pub type ParseErrors = Vec<ParseError>;

///
// Basic Implement
///
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut parser = Parser {
            lexer,
            current_token: Token::Eof,
            next_token: Token::Eof,
            errors: vec![],
        };

        parser.walk_token();
        parser.walk_token();

        parser
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

    fn walk_token(&mut self) {
        self.current_token = self.next_token.clone();
        self.next_token = self.lexer.next_token();
    }

    fn current_token_is(&mut self, tok: Token) -> bool {
        self.current_token == tok
    }

    fn next_token_is(&mut self, tok: Token) -> bool {
        self.next_token == tok
    }

    pub fn get_errors(&mut self) -> ParseErrors {
        self.errors.clone()
    }
}

///
// Stmt Parsing Implement
///
impl Parser {
    /// The entry of parse stmt
    fn parse_stmt(&mut self) -> Option<Stmt> {
        match self.current_token {
            Token::Let => self.parse_let_stmt(),
            Token::Const => self.parse_const_stmt(),
            Token::Return => self.parse_return_stmt(),
            Token::Ident(_) => match self.next_token {
                Token::Assign => self.parse_reassign_stmt(),
                _ => self.parse_expr_stmt(),
            },
            Token::Break => self.parse_break_stmt(),
            Token::Continue => self.parse_continue_stmt(),
            Token::Blank => Some(Stmt::Blank),
            _ => self.parse_expr_stmt(),
        }
    }

    /// let
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
            _ => unreachable!()
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Let(name, expr))
    }

    /// const
    fn parse_const_stmt(&mut self) -> Option<Stmt> {
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
            _ => unreachable!()
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Const(name, expr))
    }

    /// break
    fn parse_break_stmt(&mut self) -> Option<Stmt> {
        self.walk_token();

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Break)
    }

    /// continue
    fn parse_continue_stmt(&mut self) -> Option<Stmt> {
        self.walk_token();

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Continue)
    }

    /// return
    fn parse_return_stmt(&mut self) -> Option<Stmt> {
        self.walk_token();

        let expr = match self.parse_expr() {
            Some(expr) => expr,
            _ => return None,
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Return(expr))
    }

    /// expr
    fn parse_expr_stmt(&mut self) -> Option<Stmt> {
        let expr = match self.parse_expr() {
            Some(expr) => expr,
            _ => return None,
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::Expr(expr))
    }

    /// reassign
    fn parse_reassign_stmt(&mut self) -> Option<Stmt> {
        let name = match self.parse_ident() {
            Some(name) => name,
            None => return None,
        };

        if !self.next_token_is(Token::Assign) {
            return None;
        } else {
            self.walk_token();
        }

        self.walk_token();

        let expr = match self.parse_expr() {
            Some(expr) => expr,
            None => return None,
        };

        if self.next_token_is(Token::Semicolon) {
            self.walk_token();
        }

        Some(Stmt::ReAssign(name, expr))
    }
}

///
// Expr Parsing Implement
///
impl Parser {
    /// parse expr ...
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

    /// ident expr
    fn parse_ident_expr(&mut self) -> Option<Expr> {
        self.parse_ident().map(Expr::Ident)
    }

    /// int expr
    fn parse_int_expr(&mut self) -> Option<Expr> {
        match self.current_token {
            Token::Int(ref mut int) => Some(Expr::Literal(Literal::Int(*int))),
            _ => None,
        }
    }

    /// if expr
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

        Some(Expr::If {
            cond: Box::new(cond),
            consequence,
            alternative,
        })
    }

    /// while expr
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
}

///
// ...
///
impl Parser {
    /// all block stmt

    ///
    /// ident
    ///
    fn parse_ident(&mut self) -> Option<Ident> {
        match self.current_token {
            Token::Ident(ref mut ident) => Some(Ident(ident.clone())),
            _ => None,
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::ast::Expr;
    use crate::ast::Ident;
    use crate::ast::Literal;
    use crate::ast::Stmt;

    use super::Lexer;
    use super::Parser;

    ///
    // cases from 2015
    // cases from 2015
    ///

    fn check_parse_errors(parser: &mut Parser) {
        let errors = parser.get_errors();

        if errors.is_empty() {
            return;
        }

        println!("\n");

        println!("parser has {} errors", errors.len());

        for err in errors {
            println!("parse error: {:?}", err);
        }

        println!("\n");

        panic!("failed");
    }

    #[test]
    fn test_blank() {
        let input = r#"
1000;

1000;


1000;

if (x) {

    x;

}
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![
                Stmt::Expr(Expr::Literal(Literal::Int(1000))),
                Stmt::Blank,
                Stmt::Expr(Expr::Literal(Literal::Int(1000))),
                Stmt::Blank,
                Stmt::Blank,
                Stmt::Expr(Expr::Literal(Literal::Int(1000))),
                Stmt::Blank,
                Stmt::Expr(Expr::If {
                    cond: Box::new(Expr::Ident(Ident(String::from("x")))),
                    consequence: vec![
                        Stmt::Blank,
                        Stmt::Expr(Expr::Ident(Ident(String::from("x")))),
                        Stmt::Blank,
                    ],
                    alternative: None,
                }),
            ],
            program,
        );
    }

    #[test]
    fn test_let_stmt() {
        let input = r#"
let x = 5;
let y = 10;
let foobar = 838383;
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![
                Stmt::Let(Ident(String::from("x")), Expr::Literal(Literal::Int(5))),
                Stmt::Let(Ident(String::from("y")), Expr::Literal(Literal::Int(10))),
                Stmt::Let(
                    Ident(String::from("foobar")),
                    Expr::Literal(Literal::Int(838383)),
                ),
            ],
            program,
        );
    }

    #[test]
    fn test_const_stmt() {
        let input = r#"
const x = 5;
const y = 10;
const foobar = 838383;
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![
                Stmt::Const(Ident(String::from("x")), Expr::Literal(Literal::Int(5))),
                Stmt::Const(Ident(String::from("y")), Expr::Literal(Literal::Int(10))),
                Stmt::Const(
                    Ident(String::from("foobar")),
                    Expr::Literal(Literal::Int(838383)),
                ),
            ],
            program,
        );
    }

    #[test]
    fn test_reassign_stmt() {
        let input = r#"
x = 5;
y = 10;
foobar = 838383;
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![
                Stmt::ReAssign(Ident(String::from("x")), Expr::Literal(Literal::Int(5))),
                Stmt::ReAssign(Ident(String::from("y")), Expr::Literal(Literal::Int(10))),
                Stmt::ReAssign(
                    Ident(String::from("foobar")),
                    Expr::Literal(Literal::Int(838383)),
                ),
            ],
            program,
        );
    }

    #[test]
    fn test_return_stmt() {
        let input = r#"
return 5;
return 10;
return 993322;
        "#;

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![
                Stmt::Return(Expr::Literal(Literal::Int(5))),
                Stmt::Return(Expr::Literal(Literal::Int(10))),
                Stmt::Return(Expr::Literal(Literal::Int(993322))),
            ],
            program,
        );
    }

    #[test]
    fn test_ident_expr() {
        let input = "foobar;";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Stmt::Expr(Expr::Ident(Ident(String::from("foobar"))))],
            program,
        );
    }

    #[test]
    fn test_integer_literal_expr() {
        let input = "5;";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(vec![Stmt::Expr(Expr::Literal(Literal::Int(5)))], program,);
    }

    #[test]
    fn test_string_literal_expr() {}

    #[test]
    fn test_boolean_literal_expr() {}

    #[test]
    fn test_array_literal_expr() {}

    #[test]
    fn test_hash_literal_expr() {}

    #[test]
    fn test_index_expr() {}

    #[test]
    fn test_dot_index_expr() {}

    #[test]
    fn test_prefix_expr() {
        // 1004
    }

    #[test]
    fn test_infix_expr() {
        // 1004
    }

    #[test]
    fn test_if_expr() {
        // 1004
    }

    #[test]
    fn test_if_else_expr() {
        // 1004
    }

    #[test]
    fn test_func_expr() {}

    #[test]
    fn test_func_params() {}

    #[test]
    fn test_call_expr() {}

    #[test]
    fn test_operator_precedence_parsing() {
        // 1005
    }

    // self cases
    // self cases

    // #[test]
    // fn test_parser_let_stmt() {
    //     let mut lexer = Lexer::new(r"let five = 5;");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Let(ident, expr) => {
    //                 assert_eq!(ident.0, "five");
    //                 match expr {
    //                     Expr::Literal(ltr) => match ltr {
    //                         Literal::Int(i) => {
    //                             assert_eq!(*i, 5i64)
    //                         }
    //                         _ => todo!(),
    //                     },
    //                     _ => todo!(),
    //                 }
    //             }
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_return_stmt() {
    //     let mut lexer = Lexer::new(r"return a");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Return(expr) => match expr {
    //                 Expr::Ident(ltr) => {
    //                     assert_eq!(ltr.0, "a")
    //                 }
    //                 _ => todo!(),
    //             },
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_expr_stmt() {
    //     let mut lexer = Lexer::new(r"i");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Expr(expr) => match expr {
    //                 Expr::Ident(ltr) => {
    //                     assert_eq!(ltr.0, "i")
    //                 }
    //                 _ => todo!(),
    //             },
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_reassign_stmt() {
    //     let mut lexer = Lexer::new(r"i = 3");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::ReAssign(name, expr) => {
    //                 assert_eq!(name.0, "i");
    //                 match expr {
    //                     Expr::Literal(ltr) => match ltr {
    //                         Literal::Int(i) => {
    //                             assert_eq!(*i, 3i64)
    //                         }
    //                         _ => todo!(),
    //                     },
    //                     _ => todo!(),
    //                 }
    //             }
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_while_stmt() {
    //     let mut lexer = Lexer::new(r"while (w) { q = 5 }");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     println!("{:?}", program);
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Expr(expr) => match expr {
    //                 Expr::While { cond, consequence } => {
    //                     match *cond.clone() {
    //                         Expr::Ident(ident) => {
    //                             assert_eq!(ident.0, "w")
    //                         }
    //                         _ => todo!(),
    //                     }
    //                     assert_eq!(consequence.len(), 1);
    //                     match consequence.get(0) {
    //                         Some(stmt) => match stmt {
    //                             Stmt::ReAssign(ident, expr) => {
    //                                 assert_eq!(ident.0, "q");
    //                                 match expr {
    //                                     Expr::Literal(ltr) => match ltr {
    //                                         Literal::Int(i) => {
    //                                             assert_eq!(*i, 5i64)
    //                                         }
    //                                         _ => todo!(),
    //                                     },
    //                                     _ => todo!(),
    //                                 }
    //                             }
    //                             _ => todo!(),
    //                         },
    //                         _ => todo!(),
    //                     }
    //                 }
    //                 _ => todo!(),
    //             },
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_break_stmt() {
    //     let mut lexer = Lexer::new(r"while (w) { break; }");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     println!("{:?}", program);
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Expr(expr) => match expr {
    //                 Expr::While { cond, consequence } => {
    //                     match *cond.clone() {
    //                         Expr::Ident(ident) => {
    //                             assert_eq!(ident.0, "w")
    //                         }
    //                         _ => todo!(),
    //                     }
    //                     assert_eq!(consequence.len(), 1);
    //                     match consequence.get(0) {
    //                         Some(stmt) => {
    //                             assert_eq!(stmt, &Stmt::Break);
    //                         }
    //                         _ => todo!(),
    //                     }
    //                 }
    //                 _ => todo!(),
    //             },
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_continue_stmt() {
    //     let mut lexer = Lexer::new(r"while (w) { continue; }");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     println!("{:?}", program);
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Expr(expr) => match expr {
    //                 Expr::While { cond, consequence } => {
    //                     match *cond.clone() {
    //                         Expr::Ident(ident) => {
    //                             assert_eq!(ident.0, "w")
    //                         }
    //                         _ => todo!(),
    //                     }
    //                     assert_eq!(consequence.len(), 1);
    //                     match consequence.get(0) {
    //                         Some(stmt) => {
    //                             assert_eq!(stmt, &Stmt::Continue);
    //                         }
    //                         _ => todo!(),
    //                     }
    //                 }
    //                 _ => todo!(),
    //             },
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }

    // #[test]
    // fn test_parser_illegal_let_stmt() {
    //     let mut lexer = Lexer::new(r"let five 5");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     assert_eq!(program.len(), 0);
    // }

    // #[test]
    // fn test_parser_if_stmt() {
    //     let mut lexer = Lexer::new(r"if (w) { q = 5 } else { q = 1 }");
    //     let mut parser = Parser::new(lexer);
    //     let program = parser.parse();
    //     println!("{:?}", program);
    //     assert_eq!(program.len(), 1);
    //     match program.get(0) {
    //         Some(stmt) => match stmt {
    //             Stmt::Expr(expr) => match expr {
    //                 Expr::If {
    //                     cond,
    //                     consequence,
    //                     alternative,
    //                 } => {
    //                     match *cond.clone() {
    //                         Expr::Ident(ident) => {
    //                             assert_eq!(ident.0, "w")
    //                         }
    //                         _ => todo!(),
    //                     }
    //                     assert_eq!(consequence.len(), 1);
    //                     match consequence.get(0) {
    //                         Some(stmt) => match stmt {
    //                             Stmt::ReAssign(ident, expr) => {
    //                                 assert_eq!(ident.0, "q");
    //                                 match expr {
    //                                     Expr::Literal(ltr) => match ltr {
    //                                         Literal::Int(i) => {
    //                                             assert_eq!(*i, 5i64)
    //                                         }
    //                                         _ => todo!(),
    //                                     },
    //                                     _ => todo!(),
    //                                 }
    //                             }
    //                             _ => todo!(),
    //                         },
    //                         _ => todo!(),
    //                     }
    //                     if let Some(alternative) = alternative {
    //                         assert_eq!(alternative.len(), 1);
    //                         match alternative.get(0) {
    //                             Some(stmt) => match stmt {
    //                                 Stmt::ReAssign(ident, expr) => {
    //                                     assert_eq!(ident.0, "q");
    //                                     match expr {
    //                                         Expr::Literal(ltr) => match ltr {
    //                                             Literal::Int(i) => {
    //                                                 assert_eq!(*i, 1i64)
    //                                             }
    //                                             _ => todo!(),
    //                                         },
    //                                         _ => todo!(),
    //                                     }
    //                                 }
    //                                 _ => todo!(),
    //                             },
    //                             _ => todo!(),
    //                         }
    //                     }
    //                 }
    //                 _ => todo!(),
    //             },
    //             _ => todo!(),
    //         },
    //         _ => todo!(),
    //     }
    // }
}

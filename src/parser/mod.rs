use crate::ast::*;
use crate::lexer::Lexer;
use crate::token::Token;

#[derive(Debug)]
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

        let expr = match self.parse_expr(Precedence::Lowest) {
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
        let expr = match self.parse_expr(Precedence::Lowest) {
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

        let expr = match self.parse_expr(Precedence::Lowest) {
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
    fn parse_expr(&mut self, precedence: Precedence) -> Option<Expr> {
        let mut left = match self.current_token {
            Token::Ident(_) => self.parse_ident_expr(),
            Token::Int(_) => self.parse_int_expr(),
            Token::String(_) => todo!(),
            Token::Bool(_) => self.parse_bool_expr(),
            Token::LBracket => todo!(),
            Token::LBrace => todo!(),
            Token::LParen => self.parse_grouped_expr(),
            Token::Bang | Token::Minus | Token::Plus => self.parse_prefix_expr(),
            Token::If => self.parse_if_expr(),
            Token::While => self.parse_while_expr(),
            Token::Function => todo!(),
            _ => {
                // todo!();
                None
            }
        };

        //
        // recursive to parse the higher precedence right expr
        // which means "不断递归地 向 更高优先级的 右表达式 结合"
        // 例如 a + b / c 这个用例 会不断地向更右递归结合 -> 直到返回
        //
        while !self.next_token_is(Token::Semicolon) && precedence < self.next_token_precedence() {
            match self.next_token {
                Token::Plus
                | Token::Minus
                | Token::Slash
                | Token::Asterisk
                | Token::Equal
                | Token::NotEqual
                | Token::LT
                | Token::LTEQ
                | Token::GT
                | Token::GTEQ => {
                    self.walk_token();
                    left = self.parse_infix_expr(left.unwrap());
                }
                Token::LBracket => {
                    self.walk_token();
                    todo!()
                    // left = self.parse_index_expr(left.unwrap());
                }
                Token::Dot => {
                    self.walk_token();
                    todo!()
                    // left = self.parse_dot_index_expr(left.unwrap());
                }
                Token::LParen => {
                    self.walk_token();
                    todo!()
                    // left = self.parse_call_expr(left.unwrap());
                }
                _ => return left,
            }
        }
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

    /// boolean expr
    fn parse_bool_expr(&mut self) -> Option<Expr> {
        match self.current_token {
            Token::Bool(value) => Some(Expr::Literal(Literal::Bool(value))),
            _ => None,
        }
    }

    /// prefix expr
    fn parse_prefix_expr(&mut self) -> Option<Expr> {
        let prefix = match self.current_token {
            Token::Bang => Prefix::Not,
            Token::Minus => Prefix::Minus,
            Token::Plus => Prefix::Plus,
            _ => return None,
        };

        self.walk_token();

        self.parse_expr(Precedence::Prefix).map(|expr| Expr::Prefix(prefix, Box::new(expr)))
    }

    /// infix expr (which means "中缀-表达式")
    fn parse_infix_expr(&mut self, left: Expr) -> Option<Expr> {
        let infix = match self.current_token {
            Token::Plus => Infix::Plus,
            Token::Minus => Infix::Minus,
            Token::Slash => Infix::Divide,
            Token::Asterisk => Infix::Multiply,
            Token::Equal => Infix::Equal,
            Token::NotEqual => Infix::NotEqual,
            Token::LT => Infix::LT,
            Token::LTEQ => Infix::LTEQ,
            Token::GT => Infix::GT,
            Token::GTEQ => Infix::GTEQ,
            _ => return None,
        };

        let precedence = self.current_token_precedence();

        self.walk_token();

        self.parse_expr(precedence).map(|right_expr| Expr::Infix(infix, Box::new(left), Box::new(right_expr)))
    }

    /// group expr
    fn parse_grouped_expr(&mut self) -> Option<Expr> {
        self.walk_token();

        let expr = self.parse_expr(Precedence::Lowest);

        if !self.next_token_is(Token::RParen) {
            self.walk_token();
            None
        } else {
            self.walk_token();
            expr
        }
    }

    /// if expr
    fn parse_if_expr(&mut self) -> Option<Expr> {
        if !self.next_token_is(Token::LParen) {
            return None;
        }

        self.walk_token();
        self.walk_token();

        let cond = match self.parse_expr(Precedence::Lowest) {
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

        let cond = match self.parse_expr(Precedence::Lowest) {
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
// Precedence Parsing Implement (which means "运算优先级")
///
impl Parser {

    fn token_to_precedence(tok: &Token) -> Precedence {
        match tok {
            Token::Equal | Token::NotEqual => Precedence::Equals,
            Token::LT | Token::LTEQ => Precedence::LessGreater,
            Token::GT | Token::GTEQ => Precedence::LessGreater,
            Token::Plus | Token::Minus => Precedence::Sum,
            Token::Slash | Token::Asterisk => Precedence::Product,
            Token::LBracket => Precedence::Index,
            Token::Dot => Precedence::Index,
            Token::LParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }

    fn current_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(&self.current_token)
    }

    fn next_token_precedence(&mut self) -> Precedence {
        Self::token_to_precedence(&self.next_token)
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
    use crate::ast::Prefix;
    use crate::ast::Infix;

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
        let tests = vec![
            (
                "!5;",
                Stmt::Expr(Expr::Prefix(
                    Prefix::Not,
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "-15;",
                Stmt::Expr(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Literal(Literal::Int(15))),
                )),
            ),
            (
                "+15;",
                Stmt::Expr(Expr::Prefix(
                    Prefix::Plus,
                    Box::new(Expr::Literal(Literal::Int(15))),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_infix_expr() {
        let tests = vec![
            (
                "5 + 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 - 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::Minus,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 * 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 / 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::Divide,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 > 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::GT,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 < 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 == 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 != 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::NotEqual,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 >= 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::GTEQ,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
            (
                "5 <= 5;",
                Stmt::Expr(Expr::Infix(
                    Infix::LTEQ,
                    Box::new(Expr::Literal(Literal::Int(5))),
                    Box::new(Expr::Literal(Literal::Int(5))),
                )),
            ),
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
    }

    #[test]
    fn test_if_expr() {
        let input = "if (x < y) { x }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Ident(Ident(String::from("x")))),
                    Box::new(Expr::Ident(Ident(String::from("y")))),
                )),
                consequence: vec![Stmt::Expr(Expr::Ident(Ident(String::from("x"))))],
                alternative: None,
            })],
            program,
        );
    }

    #[test]
    fn test_if_else_expr() {
        let input = "if (x < y) { x } else { y }";

        let mut parser = Parser::new(Lexer::new(input));
        let program = parser.parse();

        check_parse_errors(&mut parser);
        assert_eq!(
            vec![Stmt::Expr(Expr::If {
                cond: Box::new(Expr::Infix(
                    Infix::LT,
                    Box::new(Expr::Ident(Ident(String::from("x")))),
                    Box::new(Expr::Ident(Ident(String::from("y")))),
                )),
                consequence: vec![Stmt::Expr(Expr::Ident(Ident(String::from("x"))))],
                alternative: Some(vec![Stmt::Expr(Expr::Ident(Ident(String::from("y"))))]),
            })],
            program,
        );
    }

    #[test]
    fn test_func_expr() {}

    #[test]
    fn test_func_params() {}

    #[test]
    fn test_call_expr() {}

    #[test]
    fn test_operator_precedence_parsing() {
        let tests = vec![
            (
                "-a * b",
                Stmt::Expr(Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Prefix(
                        Prefix::Minus,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("b")))),
                )),
            ),
            (
                "!-a",
                Stmt::Expr(Expr::Prefix(
                    Prefix::Not,
                    Box::new(Expr::Prefix(
                        Prefix::Minus,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                    )),
                )),
            ),
            (
                "a + b + c",
                Stmt::Expr(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a + b - c",
                Stmt::Expr(Expr::Infix(
                    Infix::Minus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a * b * c",
                Stmt::Expr(Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Infix(
                        Infix::Multiply,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a * b / c",
                Stmt::Expr(Expr::Infix(
                    Infix::Divide,
                    Box::new(Expr::Infix(
                        Infix::Multiply,
                        Box::new(Expr::Ident(Ident(String::from("a")))),
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("c")))),
                )),
            ),
            (
                "a + b / c",
                Stmt::Expr(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Ident(Ident(String::from("a")))),
                    Box::new(Expr::Infix(
                        Infix::Divide,
                        Box::new(Expr::Ident(Ident(String::from("b")))),
                        Box::new(Expr::Ident(Ident(String::from("c")))),
                    )),
                )),
            ),
            (
                "a + b * c + d / e - f",
                Stmt::Expr(Expr::Infix(
                    Infix::Minus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Infix(
                            Infix::Plus,
                            Box::new(Expr::Ident(Ident(String::from("a")))),
                            Box::new(Expr::Infix(
                                Infix::Multiply,
                                Box::new(Expr::Ident(Ident(String::from("b")))),
                                Box::new(Expr::Ident(Ident(String::from("c")))),
                            )),
                        )),
                        Box::new(Expr::Infix(
                            Infix::Divide,
                            Box::new(Expr::Ident(Ident(String::from("d")))),
                            Box::new(Expr::Ident(Ident(String::from("e")))),
                        )),
                    )),
                    Box::new(Expr::Ident(Ident(String::from("f")))),
                )),
            ),
            (
                "5 > 4 == 3 < 4",
                Stmt::Expr(Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Infix(
                        Infix::GT,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::LT,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "5 < 4 != 3 > 4",
                Stmt::Expr(Expr::Infix(
                    Infix::NotEqual,
                    Box::new(Expr::Infix(
                        Infix::LT,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::GT,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "5 >= 4 == 3 <= 4",
                Stmt::Expr(Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Infix(
                        Infix::GTEQ,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::LTEQ,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "5 <= 4 != 3 >= 4",
                Stmt::Expr(Expr::Infix(
                    Infix::NotEqual,
                    Box::new(Expr::Infix(
                        Infix::LTEQ,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                    Box::new(Expr::Infix(
                        Infix::GTEQ,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(4))),
                    )),
                )),
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                Stmt::Expr(Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::Int(4))),
                            Box::new(Expr::Literal(Literal::Int(5))),
                        )),
                    )),
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::Int(3))),
                            Box::new(Expr::Literal(Literal::Int(1))),
                        )),
                        Box::new(Expr::Infix(
                            Infix::Multiply,
                            Box::new(Expr::Literal(Literal::Int(4))),
                            Box::new(Expr::Literal(Literal::Int(5))),
                        )),
                    )),
                )),
            ),
            ("true", Stmt::Expr(Expr::Literal(Literal::Bool(true)))),
            ("false", Stmt::Expr(Expr::Literal(Literal::Bool(false)))),
            (
                "3 > 5 == false",
                Stmt::Expr(Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Infix(
                        Infix::GT,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Bool(false))),
                )),
            ),
            (
                "3 < 5 == true",
                Stmt::Expr(Expr::Infix(
                    Infix::Equal,
                    Box::new(Expr::Infix(
                        Infix::LT,
                        Box::new(Expr::Literal(Literal::Int(3))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Bool(true))),
                )),
            ),
            (
                "1 + (2 + 3) + 4",
                Stmt::Expr(Expr::Infix(
                    Infix::Plus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(1))),
                        Box::new(Expr::Infix(
                            Infix::Plus,
                            Box::new(Expr::Literal(Literal::Int(2))),
                            Box::new(Expr::Literal(Literal::Int(3))),
                        )),
                    )),
                    Box::new(Expr::Literal(Literal::Int(4))),
                )),
            ),
            (
                "(5 + 5) * 2",
                Stmt::Expr(Expr::Infix(
                    Infix::Multiply,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                    Box::new(Expr::Literal(Literal::Int(2))),
                )),
            ),
            (
                "2 / (5 + 5)",
                Stmt::Expr(Expr::Infix(
                    Infix::Divide,
                    Box::new(Expr::Literal(Literal::Int(2))),
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "-(5 + 5)",
                Stmt::Expr(Expr::Prefix(
                    Prefix::Minus,
                    Box::new(Expr::Infix(
                        Infix::Plus,
                        Box::new(Expr::Literal(Literal::Int(5))),
                        Box::new(Expr::Literal(Literal::Int(5))),
                    )),
                )),
            ),
            (
                "!(true == true)",
                Stmt::Expr(Expr::Prefix(
                    Prefix::Not,
                    Box::new(Expr::Infix(
                        Infix::Equal,
                        Box::new(Expr::Literal(Literal::Bool(true))),
                        Box::new(Expr::Literal(Literal::Bool(true))),
                    )),
                )),
            ),
            // 1005
        ];

        for (input, expect) in tests {
            let mut parser = Parser::new(Lexer::new(input));
            let program = parser.parse();

            check_parse_errors(&mut parser);
            assert_eq!(vec![expect], program);
        }
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

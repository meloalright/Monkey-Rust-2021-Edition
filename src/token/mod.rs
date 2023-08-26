#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Let,
    Ident(String),
    Assign,
    Equal,
    Int(i64),
    Illegal,
    Eof,
}


#[cfg(test)]
mod tests {
    use super::Token;
    #[test]
    fn test_say_hello() {
        assert_eq!(Token::Let, Token::Let)
    }
}
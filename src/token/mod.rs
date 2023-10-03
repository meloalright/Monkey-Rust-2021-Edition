#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Illegal,
    Eof,

    // identifiers + literals
    Ident(String),
    Int(i64),

    // operations
    Assign,
    Plus,
    Minus,
    Bang,
    Asterisk,
    Slash,

    // condition
    While,

    LT,
    GT,

    // Delimiters
    Comma,
    Semicolon,

    LParen, // (
    RParen, // )
    LBrace, // {
    RBrace, // }
    Dot,

    // Keywords
    Function,
    Let,
    True,
    False,
    If,
    Else,
    Break,
    Continue,
    Return,

    Equal,
    NotEqual,

    String(String),
    Bool(bool),

    LBracket,
    RBracket,

    Colon,

    Blank,
}


#[cfg(test)]
mod tests {
    use super::Token;
    #[test]
    fn test_say_hello() {
        assert_eq!(Token::Let, Token::Let)
    }
}
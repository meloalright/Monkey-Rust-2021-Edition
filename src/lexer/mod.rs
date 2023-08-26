use crate::token::Token;

pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    next_pos: usize,
    ch: char
}


fn is_id_start(c: char) -> bool {
    ('a'..='z').contains(&c)
    || ('A'..='Z').contains(&c)
    || c == '_'
}


fn is_id_continue(c: char) -> bool {
    ('a'..='z').contains(&c)
    || ('A'..='Z').contains(&c)
    || c == '_'
}

impl Lexer {

    pub fn new(origin_input: &str) -> Self {
        let input = origin_input.chars().collect::<Vec<char>>();
        let mut lexer = Self {
            input,
            pos: 0,
            next_pos: 0,
            ch: '\0',
        };

        lexer.walk_char();

        lexer
    }

    fn walk_char(&mut self) {
        if self.next_pos >= self.input.len() {
            self.ch = '\0';
        } else {
            self.ch = self.input[self.next_pos];
        }
        self.pos = self.next_pos;
        self.next_pos += 1;
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let tok = match self.ch {
            '=' => {
                if self.next_is('=') {
                    self.walk_char();
                    Token::Equal
                } else {
                    Token::Assign
                }
            },
            '0'..='9' => {
                self.consume_number()
            },
            '\0' => Token::Eof,
            _ => {
                if is_id_start(self.ch) {
                    return self.consume_identifier();
                } else {
                    Token::Illegal
                }
            }
        };

        self.walk_char();
        tok
    }

    fn next_is(&mut self, ch: char) -> bool {
        self.next_ch() == ch
    }

    fn next_ch(&mut self) -> char {
        if self.next_pos >= self.input.len() {
            '\0'
        } else {
            self.input[self.next_pos]
        }
    }

    fn skip_whitespace(&mut self) {
        loop {
            if {
                matches!(
                    self.ch,
                    // Usual ASCII suspects
                    '\u{0009}'   // \t
                    | '\u{000C}' // form feed
                    | '\u{000D}' // \r
                    | '\u{000B}' // vertical tab
                    | '\u{0020}' // space
            
                    // NEXT LINE from latin1
                    | '\u{0085}'
            
                    // Bidi markers
                    | '\u{200E}' // LEFT-TO-RIGHT MARK
                    | '\u{200F}' // RIGHT-TO-LEFT MARK
            
                    // Dedicated whitespace characters from Unicode
                    | '\u{2028}' // LINE SEPARATOR
                    | '\u{2029}' // PARAGRAPH SEPARATOR
                )
            } {
                self.walk_char();
            } else {
                break;
            }
        }
    }

    fn consume_identifier(&mut self) -> Token {
        let start_pos = self.pos;

        loop {
            if is_id_continue(self.ch) {
                self.walk_char();
            } else {
                break;
            }
        }

        let end_pos = self.pos;

        let literal = self.input[start_pos..end_pos].iter().collect::<String>();

        match literal.as_str() {
            "let" => Token::Let,
            _ => {
                Token::Ident(literal)
            }
        }
    }

    fn consume_number(&mut self) -> Token {
        let start_pos = self.pos;

        loop {
            match self.ch {
                '0'..='9' => {
                    self.walk_char();
                }
                _ => {
                    break;
                }
            }
        }

        let end_pos = self.pos;

        let literal = &self.input[start_pos..end_pos].iter().collect::<String>();
        Token::Int(literal.parse::<i64>().unwrap())
    }

}

#[cfg(test)]
mod tests {
    use crate::token::Token;

    use super::Lexer;
    // use crate::Token;

    #[test]
    fn test_lexer_walk() {
        let mut lexer = Lexer::new(r"let five = 5;");
        assert_eq!(lexer.pos, 0);
        assert_eq!(lexer.next_pos, 1);
        assert_eq!(lexer.ch, 'l');
    }

    #[test]
    fn test_next_token() {
        let mut lexer = Lexer::new(r"let five = 5;");
        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Ident("five".to_owned()));
        assert_eq!(lexer.next_token(), Token::Assign);
        assert_eq!(lexer.next_token(), Token::Int(5));
        assert_eq!(lexer.next_token(), Token::Eof);
    }
}
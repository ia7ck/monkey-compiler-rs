use crate::token;
use std::str::Chars;

pub struct Lexer<'a> {
    input: Chars<'a>,
    cur: Option<char>,
    peek: Option<char>,
}

impl<'a> Lexer<'a> {
    pub fn new(input: &'a str) -> Self {
        let mut lexer = Self {
            input: input.chars(),
            cur: None,
            peek: None,
        };
        lexer.read_char();
        lexer.read_char();
        lexer
    }
    fn read_char(&mut self) {
        self.cur = self.peek;
        self.peek = self.input.next();
    }
    fn skip_whitespace(&mut self) {
        while self.cur.map_or(false, |c| c.is_ascii_whitespace()) {
            self.read_char();
        }
    }
    fn read_number(&mut self) -> String {
        let mut res = String::new();
        while self.cur.map_or(false, |c| c.is_ascii_digit()) {
            res.push(self.cur.unwrap());
            self.read_char();
        }
        res
    }
    fn read_identifier(&mut self) -> String {
        let mut res = String::new();
        while self.cur.map_or(false, |c| c.is_ascii_alphabetic()) {
            res.push(self.cur.unwrap());
            self.read_char();
        }
        res
    }
}

impl<'a> Iterator for Lexer<'a> {
    type Item = token::Token;

    fn next(&mut self) -> Option<Self::Item> {
        use token::Token::*;
        self.skip_whitespace();
        let c = self.cur?;
        let token = match c {
            '=' => {
                self.read_char();
                match self.cur {
                    Some(c) if c == '=' => {
                        self.read_char();
                        EQ
                    }
                    _ => todo!(),
                }
            }
            '+' => {
                self.read_char();
                PLUS
            }
            '-' => {
                self.read_char();
                MINUS
            }
            '!' => {
                self.read_char();
                match self.cur {
                    Some(c) if c == '=' => {
                        self.read_char();
                        NEQ
                    }
                    _ => todo!(),
                }
            }
            '*' => {
                self.read_char();
                ASTERISK
            }
            '/' => {
                self.read_char();
                SLASH
            }
            '<' => {
                self.read_char();
                LT
            }
            '>' => {
                self.read_char();
                GT
            }
            ';' => {
                self.read_char();
                SEMICOLON
            }
            '(' => {
                self.read_char();
                LPAREN
            }
            ')' => {
                self.read_char();
                RPAREN
            }
            c if c.is_ascii_digit() => INT(self.read_number()),
            c if c.is_ascii_alphabetic() => {
                let literal = self.read_identifier();
                token::lookup_identifier(&literal)
            }
            c => {
                self.read_char();
                ILLEGAL(c)
            }
        };
        Some(token)
    }
}

#[cfg(test)]
mod tests {
    use crate::lexer::Lexer;
    use crate::token::Token;
    use crate::token::Token::*;

    #[test]
    fn test_next_token() {
        let input = r#"1 + 2;
34 * (5 + 6);
true; false;
1 == 1; 2 != 3; 1 < 2; 2 > 1"#;
        let tests = vec![
            INT("1".to_string()),
            PLUS,
            INT("2".to_string()),
            SEMICOLON,
            (INT("34".to_string())),
            ASTERISK,
            LPAREN,
            INT("5".to_string()),
            PLUS,
            INT("6".to_string()),
            RPAREN,
            SEMICOLON,
            TRUE,
            SEMICOLON,
            FALSE,
            SEMICOLON,
            INT("1".to_string()),
            EQ,
            INT("1".to_string()),
            SEMICOLON,
            INT("2".to_string()),
            NEQ,
            INT("3".to_string()),
            SEMICOLON,
            INT("1".to_string()),
            LT,
            INT("2".to_string()),
            SEMICOLON,
            INT("2".to_string()),
            GT,
            INT("1".to_string()),
        ];
        let lexer = Lexer::new(input);
        let tokens: Vec<Token> = lexer.collect();
        assert_eq!(tests.len(), tokens.len());
        for (expected, actual) in tests.into_iter().zip(tokens.into_iter()) {
            assert_eq!(expected, actual);
        }
    }
}

#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Token {
    ILLEGAL(char),
    EOF,

    INT(String),

    PLUS,     // +
    MINUS,    // -
    ASTERISK, // *
    SLASH,    // /

    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )

    TRUE,
    FALSE,
}

pub fn lookup_identifier(ident: &str) -> Token {
    use Token::*;
    match ident {
        "true" => TRUE,
        "false" => FALSE,
        _ => todo!(),
    }
}

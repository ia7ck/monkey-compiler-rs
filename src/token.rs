#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Token {
    ILLEGAL(char),
    EOF,

    INT(String),

    PLUS,     // +
    MINUS,    // -
    BANG,     // !
    ASTERISK, // *
    SLASH,    // /

    LT, // <
    GT, // >

    EQ,  // ==
    NEQ, // !=

    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )
    LBRACE, // {
    RBRACE, // }

    TRUE,
    FALSE,
    IF,
    ELSE,
}

pub fn lookup_identifier(ident: &str) -> Token {
    use Token::*;
    match ident {
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        _ => todo!(),
    }
}

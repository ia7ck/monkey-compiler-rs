#[derive(Eq, PartialEq, Debug)]
pub enum Token {
    ILLEGAL(char),
    EOF,

    IDENT(String), // foobar, x, y, ...
    INT(String),

    ASSIGN,   // =
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

    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
}

pub fn lookup_identifier(ident: &str) -> Token {
    use Token::*;
    match ident {
        "let" => LET,
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        _ => IDENT(ident.to_string()),
    }
}

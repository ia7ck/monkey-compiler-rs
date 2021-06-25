#[derive(PartialEq, Debug)]
pub enum Token {
    ILLEGAL(char),
    EOF,

    IDENT(String), // foobar, x, y, ...
    INT(String),
    STRING(String),

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

    COMMA,     // ,
    SEMICOLON, // ;
    COLON,     // :

    LPAREN,   // (
    RPAREN,   // )
    LBRACE,   // {
    RBRACE,   // }
    LBRACKET, // [
    RBRACKET, // ]

    FUNCTION,
    LET,
    TRUE,
    FALSE,
    IF,
    ELSE,
    RETURN,
}

pub fn lookup_identifier(ident: &str) -> Token {
    use Token::*;
    match ident {
        "fn" => FUNCTION,
        "let" => LET,
        "true" => TRUE,
        "false" => FALSE,
        "if" => IF,
        "else" => ELSE,
        "return" => RETURN,
        _ => IDENT(ident.to_string()),
    }
}

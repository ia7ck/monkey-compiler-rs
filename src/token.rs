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
}

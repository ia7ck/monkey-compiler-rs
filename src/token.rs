#[derive(Eq, PartialEq, Debug, Clone)]
pub enum Token {
    ILLEGAL(char),
    EOF,

    INT(String),

    PLUS,     // +
    ASTERISK, // *

    SEMICOLON, // ;

    LPAREN, // (
    RPAREN, // )
}

// #[derive(Eq, PartialEq, Debug, Clone)]
// pub struct Token {
//     pub(crate) r#type: TokenType,
//     literal: String,
// }
//
// impl Token {
//     pub fn new(r#type: TokenType, literal: String) -> Self {
//         Self { r#type, literal }
//     }
// }

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    MonkeyString(String),
    Boolean(bool),
    Null,
    Dummy,
}

impl Object {
    pub fn r#type(&self) -> &'static str {
        use Object::*;
        match self {
            Integer { .. } => "INTEGER",
            MonkeyString { .. } => "STRING",
            Boolean { .. } => "BOOLEAN",
            Null => "NULL",
            Dummy => unreachable!(),
        }
    }
}

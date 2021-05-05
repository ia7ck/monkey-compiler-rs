#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer { value: i64 },
    Boolean { value: bool },
    Null,
}

impl Object {
    pub fn r#type(&self) -> &'static str {
        use Object::*;
        match self {
            Integer { .. } => "INTEGER",
            Boolean { .. } => "BOOLEAN",
            Null => "NULL",
        }
    }
}

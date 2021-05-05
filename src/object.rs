#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    Boolean(bool),
    Null,
    Dummy,
}

impl Object {
    pub fn r#type(&self) -> &'static str {
        use Object::*;
        match self {
            Integer { .. } => "INTEGER",
            Boolean { .. } => "BOOLEAN",
            Null => "NULL",
            Dummy => unreachable!(),
        }
    }
}

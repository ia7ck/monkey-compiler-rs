#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    MonkeyString(String),
    Boolean(bool),
    ArrayObject(Vec<Object>),
    Null,
    Dummy,
}

impl Object {
    pub fn r#type(&self) -> &'static str {
        use Object::*;
        match self {
            Integer(..) => "INTEGER",
            MonkeyString(..) => "STRING",
            Boolean(..) => "BOOLEAN",
            ArrayObject(..) => "ARRAY",
            Null => "NULL",
            Dummy => unreachable!(),
        }
    }
}

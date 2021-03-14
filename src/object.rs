#[derive(Debug, Clone)]
pub enum Object {
    Integer { value: i64 },
}

impl Object {
    pub fn r#type(&self) -> &'static str {
        match self {
            Object::Integer { .. } => "INTEGER",
        }
    }
}

use anyhow::{bail, Result};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    MonkeyString(String),
    Boolean(bool),
    ArrayObject(Vec<Rc<Object>>),
    HashObject(HashMap<u64, Rc<Object>>),
    Null,
    Dummy,
}

impl Object {
    pub fn calculate_hash(&self) -> Result<u64> {
        let mut s = DefaultHasher::new();
        mem::discriminant(self).hash(&mut s);
        match self {
            Object::Integer(val) => {
                val.hash(&mut s);
            }
            Object::MonkeyString(val) => {
                val.hash(&mut s);
            }
            Object::Boolean(val) => {
                val.hash(&mut s);
            }
            obj => {
                bail!("unusable as hash key: {}", obj.r#type());
            }
        }
        Ok(s.finish())
    }
}

impl Object {
    pub fn r#type(&self) -> &'static str {
        use Object::*;
        match self {
            Integer(..) => "INTEGER",
            MonkeyString(..) => "STRING",
            Boolean(..) => "BOOLEAN",
            ArrayObject(..) => "ARRAY",
            HashObject(..) => "HASH",
            Null => "NULL",
            Dummy => unreachable!(),
        }
    }
}

use crate::code::Instructions;
use anyhow::{bail, Result};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem;
use std::rc::Rc;

#[derive(Debug, Clone, PartialEq)]
pub struct HashPair {
    key: Rc<Object>,
    value: Rc<Object>,
}

impl HashPair {
    pub fn new(key: Rc<Object>, value: Rc<Object>) -> Self {
        Self { key, value }
    }
    pub fn value(&self) -> Rc<Object> {
        Rc::clone(&self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    MonkeyString(String),
    Boolean(bool),
    ArrayObject(Vec<Rc<Object>>),
    HashObject(HashMap<u64, Rc<HashPair>>),
    CompiledFunctionObject(Rc<CompiledFunctionObject>),
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
            CompiledFunctionObject(..) => "COMPILED_FUNCTION",
            Null => "NULL",
            Dummy => unreachable!(),
        }
    }
}

impl Display for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        use Object::*;
        match self {
            Integer(i) => write!(f, "{}", i),
            MonkeyString(s) => write!(f, "\"{}\"", s),
            Boolean(b) => write!(f, "{}", b),
            ArrayObject(elements) => {
                write!(f, "[")?;
                let mut elements = elements.iter();
                if let Some(first) = elements.next() {
                    write!(f, "{}", first)?;
                }
                for e in elements {
                    write!(f, ", {}", e)?;
                }
                write!(f, "]")
            }
            HashObject(hash) => {
                write!(f, "{{")?;
                let mut pairs = hash.values();
                if let Some(first) = pairs.next() {
                    write!(f, "{}: {}", first.key, first.value)?;
                }
                for p in pairs {
                    write!(f, ", {}: {}", p.key, p.value)?;
                }
                write!(f, "}}")
            }
            CompiledFunctionObject(..) => write!(f, "CompiledFunction[{:p}]", self),
            Null => write!(f, "NULL"),
            Dummy => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompiledFunctionObject {
    instructions: Instructions,
    num_locals: usize,
}

impl CompiledFunctionObject {
    pub fn new(instructions: Instructions, num_locals: usize) -> Self {
        Self {
            instructions,
            num_locals,
        }
    }
    pub fn instructions(&self) -> &Instructions {
        &self.instructions
    }
    pub fn num_locals(&self) -> usize {
        self.num_locals
    }
}

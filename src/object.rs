use once_cell::sync::Lazy;

use crate::code::Instructions;
use anyhow::{bail, ensure, Result};
use std::collections::hash_map::DefaultHasher;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::mem;

#[derive(Debug, Clone, PartialEq)]
pub struct HashPair {
    key: Object,
    value: Object,
}

impl HashPair {
    pub fn new(key: Object, value: Object) -> Self {
        Self { key, value }
    }
    pub fn value(&self) -> &Object {
        &self.value
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(i64),
    MonkeyString(String),
    Boolean(bool),
    ArrayObject(Vec<Object>),
    HashObject(HashMap<u64, HashPair>),
    CompiledFunctionObject(CompiledFunctionObject),
    BuiltinFunction(Builtin),
    ClosureObject(Closure),
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
            BuiltinFunction(..) => "BUILTIN_FUNCTION",
            ClosureObject(..) => "CLOSURE",
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
            BuiltinFunction(func) => write!(f, "{}", func.name()),
            ClosureObject(..) => write!(f, "Closure[{:p}]", self),
            Null => write!(f, "NULL"),
            Dummy => unreachable!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct CompiledFunctionObject {
    instructions: Instructions,
    num_locals: usize,
    num_parameters: usize,
}

impl CompiledFunctionObject {
    pub fn new(instructions: Instructions, num_locals: usize, num_parameters: usize) -> Self {
        Self {
            instructions,
            num_locals,
            num_parameters,
        }
    }
    pub fn instructions(&self) -> &Instructions {
        &self.instructions
    }
    pub fn num_locals(&self) -> usize {
        self.num_locals
    }
    pub fn num_parameters(&self) -> usize {
        self.num_parameters
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Builtin {
    Len,
    Puts,
    First,
    Last,
    Rest,
    Push,
}

pub static BUILTINS: Lazy<Vec<Builtin>> = Lazy::new(|| {
    use Builtin::*;
    vec![Len, Puts, First, Last, Rest, Push]
});

impl Builtin {
    pub fn name(&self) -> &'static str {
        match self {
            Builtin::Len => "len",
            Builtin::Puts => "puts",
            Builtin::First => "first",
            Builtin::Last => "last",
            Builtin::Rest => "rest",
            Builtin::Push => "push",
        }
    }
    pub fn call(&self, arguments: &[Object]) -> Result<Option<Object>> {
        match self {
            Builtin::Len => {
                ensure!(
                    arguments.len() == 1,
                    "wrong number of arguments. got={}, want=1",
                    arguments.len()
                );
                match &arguments[0] {
                    Object::MonkeyString(s) => Ok(Some(Object::Integer(s.len() as i64))),
                    Object::ArrayObject(a) => Ok(Some(Object::Integer(a.len() as i64))),
                    obj => {
                        bail!("argument to `len` not supported, got {}", obj.r#type());
                    }
                }
            }
            Builtin::Puts => {
                for arg in arguments {
                    println!("{}", arg);
                }
                Ok(None)
            }
            Builtin::First => {
                ensure!(
                    arguments.len() == 1,
                    "wrong number of arguments. got={}, want=1",
                    arguments.len()
                );
                match &arguments[0] {
                    Object::ArrayObject(a) => {
                        if let Some(first) = a.first() {
                            Ok(Some(first.clone()))
                        } else {
                            Ok(None)
                        }
                    }
                    obj => {
                        bail!("argument to `first` must be ARRAY, got {}", obj.r#type());
                    }
                }
            }
            Builtin::Last => {
                ensure!(
                    arguments.len() == 1,
                    "wrong number of arguments. got={}, want=1",
                    arguments.len()
                );
                match &arguments[0] {
                    Object::ArrayObject(a) => {
                        if let Some(last) = a.last() {
                            Ok(Some(last.clone()))
                        } else {
                            Ok(None)
                        }
                    }
                    obj => {
                        bail!("argument to `last` must be ARRAY, got {}", obj.r#type());
                    }
                }
            }
            Builtin::Rest => {
                ensure!(
                    arguments.len() == 1,
                    "wrong number of arguments. got={}, want=1",
                    arguments.len()
                );
                match &arguments[0] {
                    Object::ArrayObject(a) => {
                        if a.is_empty() {
                            Ok(None)
                        } else {
                            let rest: Vec<Object> = a[1..].to_vec();
                            Ok(Some(Object::ArrayObject(rest)))
                        }
                    }
                    obj => {
                        bail!("argument to `rest` must be ARRAY, got {}", obj.r#type());
                    }
                }
            }
            Builtin::Push => {
                ensure!(
                    arguments.len() == 2,
                    "wrong number of arguments. got={}, want=2",
                    arguments.len()
                );
                match &arguments[0] {
                    Object::ArrayObject(a) => {
                        let mut elements: Vec<Object> = a.to_vec();
                        elements.push(arguments[1].clone());
                        Ok(Some(Object::ArrayObject(elements)))
                    }
                    obj => {
                        bail!("argument to `push` must be ARRAY, got {}", obj.r#type());
                    }
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Closure {
    function: CompiledFunctionObject,
    free: Vec<Object>,
}

impl Closure {
    pub fn new(function: CompiledFunctionObject, free: Vec<Object>) -> Self {
        Self { function, free }
    }
    pub fn function(&self) -> &CompiledFunctionObject {
        &self.function
    }
    pub fn free(&self) -> &Vec<Object> {
        &self.free
    }
}

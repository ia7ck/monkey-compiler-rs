use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolScope {
    GlobalScope,
    LocalScope,
}

#[derive(Clone, PartialEq, Debug)]
pub struct Symbol {
    name: String,
    scope: SymbolScope,
    index: usize,
}

impl Symbol {
    pub fn index(&self) -> usize {
        self.index
    }
    pub fn scope(&self) -> &SymbolScope {
        &self.scope
    }
}

#[derive(Clone)]
pub struct SymbolTable {
    outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
    pub fn new_enclosed_symbol_table(outer: Box<SymbolTable>) -> Self {
        Self {
            outer: Some(outer),
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
    pub fn define(&mut self, name: &str) -> Symbol {
        let scope = match self.outer {
            Some(_) => SymbolScope::LocalScope,
            None => SymbolScope::GlobalScope,
        };
        let symbol = Symbol {
            name: name.to_string(),
            scope,
            index: self.num_definitions,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        if let Some(symbol) = self.store.get(name) {
            return Some(symbol);
        }
        self.outer.as_ref().and_then(|table| table.resolve(name))
    }
    pub fn outer(&self) -> Option<&SymbolTable> {
        self.outer.as_deref()
    }
    pub fn num_definitions(&self) -> usize {
        self.num_definitions
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol_table::{Symbol, SymbolScope, SymbolTable};

    #[test]
    fn test_define() {
        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(
            a,
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::GlobalScope,
                index: 0
            }
        );

        let b = global.define("b");
        assert_eq!(
            b,
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::GlobalScope,
                index: 1
            }
        );
    }

    #[test]
    fn test_resolve_global() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");
        let tests = vec![
            (
                "a",
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 1,
                },
            ),
        ];
        for (name, symbol) in tests {
            let res = global.resolve(name).unwrap();
            assert_eq!(res, &symbol);
        }
    }

    #[test]
    fn test_resolve_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");
        let global = Box::new(global);

        let mut local = SymbolTable::new_enclosed_symbol_table(global);
        local.define("c");
        local.define("d");

        let tests = vec![
            (
                "a",
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".to_string(),
                    scope: SymbolScope::LocalScope,
                    index: 0,
                },
            ),
            (
                "d",
                Symbol {
                    name: "d".to_string(),
                    scope: SymbolScope::LocalScope,
                    index: 1,
                },
            ),
        ];
        for (name, symbol) in tests {
            let res = local.resolve(name).unwrap();
            assert_eq!(res, &symbol);
        }
    }

    #[test]
    fn test_resolve_nested_local() {
        let mut global = SymbolTable::new();
        global.define("a");
        global.define("b");
        let global = Box::new(global);

        let mut first_local = SymbolTable::new_enclosed_symbol_table(global);
        first_local.define("c");
        first_local.define("d");
        let first_local = Box::new(first_local);

        let mut second_local = SymbolTable::new_enclosed_symbol_table(first_local.clone());
        second_local.define("e");
        second_local.define("f");

        let first_tests = vec![
            (
                "a",
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 1,
                },
            ),
            (
                "c",
                Symbol {
                    name: "c".to_string(),
                    scope: SymbolScope::LocalScope,
                    index: 0,
                },
            ),
            (
                "d",
                Symbol {
                    name: "d".to_string(),
                    scope: SymbolScope::LocalScope,
                    index: 1,
                },
            ),
        ];

        let second_tests = vec![
            (
                "a",
                Symbol {
                    name: "a".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 0,
                },
            ),
            (
                "b",
                Symbol {
                    name: "b".to_string(),
                    scope: SymbolScope::GlobalScope,
                    index: 1,
                },
            ),
            (
                "e",
                Symbol {
                    name: "e".to_string(),
                    scope: SymbolScope::LocalScope,
                    index: 0,
                },
            ),
            (
                "f",
                Symbol {
                    name: "f".to_string(),
                    scope: SymbolScope::LocalScope,
                    index: 1,
                },
            ),
        ];

        for (name, symbol) in first_tests {
            let res = first_local.resolve(name).unwrap();
            assert_eq!(res, &symbol);
        }

        for (name, symbol) in second_tests {
            let res = second_local.resolve(name).unwrap();
            assert_eq!(res, &symbol);
        }
    }
}

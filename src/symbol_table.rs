use std::collections::HashMap;

#[derive(Clone, PartialEq, Debug)]
pub enum SymbolScope {
    GlobalScope,
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
}

#[derive(Clone)]
pub struct SymbolTable {
    store: HashMap<String, Symbol>,
    num_definitions: usize,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            store: HashMap::new(),
            num_definitions: 0,
        }
    }
    pub fn define(&mut self, name: &str) -> Symbol {
        let symbol = Symbol {
            name: name.to_string(),
            scope: SymbolScope::GlobalScope,
            index: self.num_definitions,
        };
        self.store.insert(name.to_string(), symbol.clone());
        self.num_definitions += 1;
        symbol
    }
    pub fn resolve(&self, name: &str) -> Option<&Symbol> {
        self.store.get(name)
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
}

use crate::object::BUILTINS;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Clone, Copy, PartialEq, Debug)]
pub enum SymbolScope {
    GlobalScope,
    LocalScope,
    BuiltinScope,
    FreeScope,
    FunctionScope,
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
    pub fn scope(&self) -> SymbolScope {
        self.scope
    }
}

#[derive(Clone)]
pub struct SymbolTable {
    outer: Option<Box<SymbolTable>>,

    store: HashMap<String, Rc<Symbol>>,
    num_definitions: usize,

    free_symbols: Vec<Rc<Symbol>>,
}

impl SymbolTable {
    pub fn new() -> Self {
        Self {
            outer: None,
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }
    pub fn new_enclosed_symbol_table(outer: Box<SymbolTable>) -> Self {
        Self {
            outer: Some(outer),
            store: HashMap::new(),
            num_definitions: 0,
            free_symbols: Vec::new(),
        }
    }
    pub fn define(&mut self, name: &str) -> Rc<Symbol> {
        let scope = match self.outer {
            Some(_) => SymbolScope::LocalScope,
            None => SymbolScope::GlobalScope,
        };
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope,
            index: self.num_definitions,
        });
        self.store.insert(name.to_string(), Rc::clone(&symbol));
        self.num_definitions += 1;
        symbol
    }
    pub fn define_builtin(&mut self, index: usize, name: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope: SymbolScope::BuiltinScope,
            index,
        });
        self.store.insert(name.to_string(), Rc::clone(&symbol));
        symbol
    }
    pub fn define_builtins(&mut self) {
        for (i, builtin) in BUILTINS.iter().enumerate() {
            self.define_builtin(i, builtin.name());
        }
    }
    pub fn define_free(&mut self, original: Rc<Symbol>) -> Rc<Symbol> {
        self.free_symbols.push(Rc::clone(&original));

        let symbol = Rc::new(Symbol {
            name: original.name.clone(),
            scope: SymbolScope::FreeScope,
            index: self.free_symbols.len() - 1,
        });

        self.store.insert(symbol.name.clone(), Rc::clone(&symbol));
        symbol
    }
    pub fn define_function_name(&mut self, name: &str) -> Rc<Symbol> {
        let symbol = Rc::new(Symbol {
            name: name.to_string(),
            scope: SymbolScope::FunctionScope,
            index: 3141592, // does not matter
        });
        self.store.insert(name.to_string(), Rc::clone(&symbol));
        symbol
    }
    pub fn resolve(&mut self, name: &str) -> Option<Rc<Symbol>> {
        match self.store.get(name) {
            None => {
                let outer = self.outer.as_mut()?;
                let symbol = outer.resolve(name)?;
                match symbol.scope {
                    SymbolScope::GlobalScope | SymbolScope::BuiltinScope => Some(symbol),
                    _ => Some(self.define_free(symbol)),
                }
            }
            Some(symbol) => Some(Rc::clone(symbol)),
        }
    }
    pub fn outer(&self) -> Option<&SymbolTable> {
        self.outer.as_deref()
    }
    pub fn num_definitions(&self) -> usize {
        self.num_definitions
    }
    pub fn free_symbols(&self) -> &Vec<Rc<Symbol>> {
        &self.free_symbols
    }
}

#[cfg(test)]
mod tests {
    use crate::symbol_table::{Symbol, SymbolScope, SymbolTable};
    use std::rc::Rc;

    #[test]
    fn test_define() {
        let mut global = SymbolTable::new();

        let a = global.define("a");
        assert_eq!(
            a,
            Rc::new(Symbol {
                name: "a".to_string(),
                scope: SymbolScope::GlobalScope,
                index: 0
            })
        );

        let b = global.define("b");
        assert_eq!(
            b,
            Rc::new(Symbol {
                name: "b".to_string(),
                scope: SymbolScope::GlobalScope,
                index: 1
            })
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
            assert_eq!(global.resolve(name), Some(Rc::new(symbol)));
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
            assert_eq!(local.resolve(name), Some(Rc::new(symbol)));
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
        let mut first_local = Box::new(first_local);

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
            assert_eq!(first_local.resolve(name), Some(Rc::new(symbol)));
        }

        for (name, symbol) in second_tests {
            assert_eq!(second_local.resolve(name), Some(Rc::new(symbol)));
        }
    }

    #[test]
    fn test_define_resolve_builtins() {
        let global = SymbolTable::new();
        let mut global = Box::new(global);

        let expected_symbols = vec![
            Symbol {
                name: "a".to_string(),
                scope: SymbolScope::BuiltinScope,
                index: 0,
            },
            Symbol {
                name: "b".to_string(),
                scope: SymbolScope::BuiltinScope,
                index: 1,
            },
            Symbol {
                name: "c".to_string(),
                scope: SymbolScope::BuiltinScope,
                index: 2,
            },
            Symbol {
                name: "d".to_string(),
                scope: SymbolScope::BuiltinScope,
                index: 3,
            },
        ];

        for (i, sym) in expected_symbols.iter().enumerate() {
            global.define_builtin(i, &sym.name);
        }

        let first_local = SymbolTable::new_enclosed_symbol_table(global.clone());
        let first_local = Box::new(first_local);
        let second_local = SymbolTable::new_enclosed_symbol_table(first_local.clone());
        let second_local = Box::new(second_local);

        for table in &[global, first_local, second_local] {
            let mut table = table.clone();
            for sym in &expected_symbols {
                let actual = table.resolve(&sym.name);
                assert_eq!(Some(Rc::new(sym.clone())), actual);
            }
        }
    }

    #[test]
    fn test_resolve_free() {
        let global = SymbolTable::new();
        let mut global = Box::new(global);
        global.define("a");
        global.define("b");

        let first_local = SymbolTable::new_enclosed_symbol_table(global.clone());
        let mut first_local = Box::new(first_local);
        first_local.define("c");
        first_local.define("d");

        let second_local = SymbolTable::new_enclosed_symbol_table(first_local.clone());
        let mut second_local = Box::new(second_local);
        second_local.define("e");
        second_local.define("f");

        let tests = vec![
            (
                first_local,
                vec![
                    Symbol {
                        name: "a".to_string(),
                        scope: SymbolScope::GlobalScope,
                        index: 0,
                    },
                    Symbol {
                        name: "b".to_string(),
                        scope: SymbolScope::GlobalScope,
                        index: 1,
                    },
                    Symbol {
                        name: "c".to_string(),
                        scope: SymbolScope::LocalScope,
                        index: 0,
                    },
                    Symbol {
                        name: "d".to_string(),
                        scope: SymbolScope::LocalScope,
                        index: 1,
                    },
                ],
                vec![],
            ),
            (
                second_local,
                vec![
                    Symbol {
                        name: "a".to_string(),
                        scope: SymbolScope::GlobalScope,
                        index: 0,
                    },
                    Symbol {
                        name: "b".to_string(),
                        scope: SymbolScope::GlobalScope,
                        index: 1,
                    },
                    Symbol {
                        name: "c".to_string(),
                        scope: SymbolScope::FreeScope,
                        index: 0,
                    },
                    Symbol {
                        name: "d".to_string(),
                        scope: SymbolScope::FreeScope,
                        index: 1,
                    },
                    Symbol {
                        name: "e".to_string(),
                        scope: SymbolScope::LocalScope,
                        index: 0,
                    },
                    Symbol {
                        name: "f".to_string(),
                        scope: SymbolScope::LocalScope,
                        index: 1,
                    },
                ],
                vec![
                    Symbol {
                        name: "c".to_string(),
                        scope: SymbolScope::LocalScope,
                        index: 0,
                    },
                    Symbol {
                        name: "d".to_string(),
                        scope: SymbolScope::LocalScope,
                        index: 1,
                    },
                ],
            ),
        ];
        for (mut table, expected_symbols, expected_free_symbols) in tests {
            for sym in &expected_symbols {
                assert_eq!(Some(Rc::new(sym.clone())), table.resolve(&sym.name));
            }

            assert_eq!(expected_free_symbols.len(), table.free_symbols.len());
            for (i, sym) in expected_free_symbols.iter().enumerate() {
                assert_eq!(Rc::new(sym.clone()), table.free_symbols[i]);
            }
        }
    }

    #[test]
    fn test_define_and_resolve_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");

        let expected = Symbol {
            name: "a".to_string(),
            scope: SymbolScope::FunctionScope,
            index: 3141592,
        };

        assert_eq!(Some(Rc::new(expected)), global.resolve("a"));
    }

    #[test]
    fn test_shadowing_function_name() {
        let mut global = SymbolTable::new();
        global.define_function_name("a");
        global.define("a");

        let expected = Symbol {
            name: "a".to_string(),
            scope: SymbolScope::GlobalScope,
            index: 0,
        };

        assert_eq!(Some(Rc::new(expected)), global.resolve("a"));
    }
}

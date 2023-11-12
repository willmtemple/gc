// use std::cell::OnceCell;

use std::{ops::Deref, sync::Arc, sync::OnceLock};

use hamt::HamtMap;

use crate::{
    ast::Attribute,
    value2::{Object, Symbol},
};

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub struct Scope {
    data: HamtMap<Arc<str>, Arc<Binding>>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new()
    }
}

impl Scope {
    pub fn new() -> Self {
        Self {
            data: HamtMap::new(),
        }
    }

    // pub(crate) fn dump(&self) {
    //     for (n, binding) in self.data.iter() {
    //         println!("{}: {:?}", n, binding);
    //     }
    // }

    pub fn resolve(&self, sym: &Symbol) -> Option<&Arc<Object>> {
        self.get_binding(sym)
            .and_then(|binding| binding.value.get())
    }

    pub fn get_binding(&self, sym: &Symbol) -> Option<&Binding> {
        self.data.get(&sym.name).map(|v| v.deref())
    }

    pub fn define(&mut self, symbol: &Symbol) -> Arc<Binding> {
        let value = OnceLock::new();

        let binding = Arc::new(Binding {
            symbol: symbol.clone(),
            value,
            attributes: HamtMap::new(),
        });

        self.data = self.data.insert(symbol.name.clone(), binding.clone());

        binding
    }

    pub fn define_with_attrs(
        &mut self,
        symbol: &Symbol,
        attributes: HamtMap<Symbol, Attribute>,
    ) -> Arc<Binding> {
        let value = OnceLock::new();

        let binding = Arc::new(Binding {
            symbol: symbol.clone(),
            value,
            attributes,
        });

        self.data = self.data.insert(symbol.name.clone(), binding.clone());

        binding
    }
}

#[derive(Clone, Debug, Eq, PartialEq)]
pub struct Binding {
    pub symbol: Symbol,
    pub value: OnceLock<Arc<Object>>,
    pub attributes: HamtMap<Symbol, Attribute>,
    // metadata: HamtMap<Symbol, Value>,
}

impl core::hash::Hash for Binding {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        // Hash the once cell as if it were an option
        self.value.get().hash(state);
    }
}

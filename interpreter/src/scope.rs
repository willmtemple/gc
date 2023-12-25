// use std::cell::OnceCell;

use std::{any::TypeId, ops::Deref, sync::Arc, sync::OnceLock};

use hamt::{config::CloningConfig, HamtMap};

use crate::{
    value::{Object, Symbol},
    InterpreterError, InterpreterResult,
};

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct InternedSymbol {
    id: usize,
    name: Symbol,
}

impl InternedSymbol {
    pub fn name(&self) -> Arc<str> {
        self.name.name.clone()
    }
}

pub struct Scope {
    data: HamtMap<Symbol, InternedSymbol, CloningConfig>,
    counter: usize,
}

impl Scope {
    pub fn new() -> Self {
        Self {
            data: HamtMap::new(),
            counter: 0,
        }
    }

    pub fn define(&mut self, name: Symbol) -> InternedSymbol {
        let symbol = InternedSymbol {
            id: self.counter,
            name: name.clone(),
        };

        self.data = self.data.insert(name, symbol);

        self.counter += 1;

        symbol
    }

    pub fn intern(&self, name: &Symbol) -> Option<InternedSymbol> {
        self.data.get(name).cloned()
    }
}

impl Clone for Scope {
    fn clone(&self) -> Self {
        Self {
            data: self.data.clone(),
            counter: self.counter.clone(),
        }
    }
}

#[derive(Clone)]
pub struct BindingTemplate {
    entries: Vec<(InternedSymbol, Binder)>,
}

#[derive(Debug, Clone)]
pub struct LexicalEnvironment {
    environ: HamtMap<InternedSymbol, Binder, CloningConfig>,
}

impl LexicalEnvironment {
    pub fn empty() -> Self {
        Self {
            environ: HamtMap::new(),
        }
    }
    pub fn with_intake(&self, template: BindingTemplate) -> Self {
        let mut next_environ = self.environ.clone();

        for (sym, binder) in template.entries.iter() {
            next_environ = next_environ.insert(sym.clone(), binder.clone());
        }

        Self {
            environ: next_environ,
        }
    }

    pub fn get_binding(&self, name: &InternedSymbol) -> Option<&dyn Binding> {
        self.environ.get(name).map(|v| v.binding())
    }
}

#[derive(Clone, Debug)]
pub struct OldScope {
    data: HamtMap<Symbol, Binder>,
}

impl Default for OldScope {
    fn default() -> Self {
        Self::new()
    }
}

impl OldScope {
    pub fn new() -> Self {
        Self {
            data: HamtMap::new(),
        }
    }

    pub(crate) fn dump(&self) {
        for (n, binding) in self.data.iter() {
            println!("{:?}: {:?}", n, binding);
        }
    }

    pub fn get_binding(&self, name: &Symbol) -> Option<Binder> {
        self.data.get(&name).cloned()
    }

    pub fn define<B: Binding>(&mut self, name: Symbol, binder: B) -> Binder {
        let binding = Arc::<B>::new(binder) as Arc<dyn Binding>;

        let binder = Binder {
            _impl: binding,
            type_id: TypeId::of::<B>(),
        };

        self.data = self.data.insert(name, binder.clone());

        binder
    }
}

#[derive(Debug)]
pub struct Binder {
    type_id: TypeId,
    _impl: Arc<dyn Binding>,
}

impl Binder {
    pub fn new<B: Binding>(v: B) -> Self {
        Self {
            type_id: TypeId::of::<B>(),
            _impl: Arc::new(v),
        }
    }

    pub fn is<B: Binding>(&self) -> bool {
        self.type_id == TypeId::of::<B>()
    }

    pub fn binding(&self) -> &dyn Binding {
        self._impl.deref()
    }
}

impl Clone for Binder {
    fn clone(&self) -> Self {
        self._impl.dyn_clone()
    }
}

pub trait Binding: core::fmt::Debug {
    fn name(&self) -> &Symbol;

    fn set(&self, value: Arc<Object>) -> InterpreterResult<()>;

    fn get(&self) -> Option<Arc<Object>>;

    fn dyn_clone(&self) -> Binder;
}

#[derive(Clone, Debug)]
pub struct Let {
    name: Symbol,
    lock: OnceLock<Arc<Object>>,
}

impl Let {
    pub fn uninitialized(name: Symbol) -> Self {
        Self {
            name,
            lock: OnceLock::new(),
        }
    }

    pub fn with_value(name: Symbol, value: Arc<Object>) -> Self {
        let lock = OnceLock::new();

        lock.set(value.clone());

        Self { name, lock }
    }
}

impl Binding for Let {
    fn name(&self) -> &Symbol {
        &self.name
    }

    fn set(&self, value: Arc<Object>) -> InterpreterResult<()> {
        match self.lock.set(value) {
            Ok(_) => InterpreterResult::Value(()),
            Err(_) => InterpreterResult::Error(InterpreterError::Rebind(self.name.clone())),
        }
    }

    fn get(&self) -> Option<Arc<Object>> {
        self.lock.get().cloned()
    }

    fn dyn_clone(&self) -> Binder {
        Binder::new(self.clone())
    }
}

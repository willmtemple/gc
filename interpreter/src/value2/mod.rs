use std::{
    any::TypeId,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    sync::Arc,
};

use crate::{Interpreter, InterpreterError, InterpreterResult};

mod block;
mod function;
mod hamt;
mod native;
mod number;
mod primitive;
mod sigil;
mod string;
mod symbol;
mod tuple;
mod r#type;

pub use block::Block;
pub use function::Function;
pub use hamt::{Map, Set, Slice, Vec};
pub use native::NativeObject;
pub use sigil::Sigil;
pub use string::String;
pub use symbol::Symbol;
pub use tuple::Tuple;

use self::r#type::Type;

pub struct Object {
    ptr: usize,
    type_id: TypeId,
    vtable: &'static ObjectVTable,
}

impl Object {
    pub fn new<V: Value + 'static>(v: V) -> Self {
        Self {
            ptr: Box::into_raw(Box::new(v)) as usize,
            type_id: V::TYPE_ID,
            vtable: &ObjectVTable {
                name: V::NAME,
                _type: || Type::<V>::new().to_object(),

                egal: |this, other| {
                    if TypeId::of::<V>() != other.type_id {
                        return false;
                    }

                    V::egal(unsafe { &*(this as *const V) }, unsafe {
                        &*(other.ptr as *const V)
                    })
                },
                hash: |this, state| {
                    state.write_u64(
                        V::d_hash(unsafe { &*(this as *const V) })
                            .expect_result()
                            .unwrap(),
                    )
                },

                call: |value, interpreter, args| {
                    V::call(unsafe { &*(value as *const V) }, interpreter, args)
                },
                to_string: |value, interpreter| {
                    V::to_string(unsafe { &*(value as *const V) }, interpreter)
                },
                get: |value, interpreter, key| {
                    V::get(unsafe { &*(value as *const V) }, interpreter, key)
                },

                drop: |value| {
                    drop(unsafe { Box::from_raw(value as *mut V) });
                },
            },
        }
    }

    pub fn type_name(&self) -> &'static str {
        self.vtable.name
    }

    pub fn type_id(&self) -> TypeId {
        self.type_id
    }

    pub fn call(&self, interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
        (self.vtable.call)(self.ptr as *const (), interpreter, args)
    }

    pub fn to_string(&self, interpreter: &mut Interpreter) -> InterpreterResult {
        (self.vtable.to_string)(self.ptr as *const (), interpreter)
    }

    pub fn get(&self, interpreter: &mut Interpreter, key: Arc<Object>) -> InterpreterResult {
        (self.vtable.get)(self.ptr as *const (), interpreter, key)
    }

    #[inline(always)]
    pub fn is<V: Value + 'static>(&self) -> bool {
        self.type_id == TypeId::of::<V>()
    }

    pub fn downcast<V: Value + 'static>(&self) -> Option<&V> {
        if self.is::<V>() {
            Some(unsafe { &*(self.ptr as *const V) })
        } else {
            None
        }
    }

    pub fn with_downcast<V: Value + 'static, T>(&self, f: impl FnOnce(&V) -> T) -> Option<T> {
        self.downcast().map(f)
    }
}

impl PartialEq for Object {
    fn eq(&self, other: &Self) -> bool {
        (self.vtable.egal)(self.ptr as *const (), other)
    }
}

impl Eq for Object {}

impl Hash for Object {
    fn hash<H: Hasher>(&self, state: &mut H) {
        (self.vtable.hash)(self.ptr as *const (), state)
    }
}

impl Debug for Object {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "{{{}@{:p}}}", self.type_name(), self.ptr as *const ())
    }
}

impl Drop for Object {
    fn drop(&mut self) {
        (self.vtable.drop)(self.ptr as *const ());
    }
}

pub struct ObjectVTable {
    pub name: &'static str,
    pub _type: fn() -> Arc<Object>,

    pub egal: fn(*const (), &Object) -> bool,
    pub hash: fn(*const (), &mut dyn Hasher),

    pub call: fn(*const (), &mut Interpreter, Slice) -> InterpreterResult,
    pub to_string: fn(*const (), &mut Interpreter) -> InterpreterResult,
    pub get: fn(*const (), &mut Interpreter, Arc<Object>) -> InterpreterResult,

    pub drop: fn(*const ()),
}

pub trait Value: Sized + 'static {
    const NAME: &'static str;
    const TYPE_ID: TypeId = TypeId::of::<Self>();

    fn egal(&self, other: &Self) -> bool;

    fn d_hash(&self) -> InterpreterResult<u64> {
        InterpreterResult::Error(InterpreterError::ProtocolNotImplemented("Hash", Self::NAME))
    }

    #[allow(unused_variables)]
    fn call(&self, interpreter: &mut Interpreter, args: Slice) -> InterpreterResult {
        InterpreterResult::Error(InterpreterError::UncallableValue(().to_object()))
    }

    #[allow(unused_variables)]
    fn to_string(&self, interpreter: &mut Interpreter) -> InterpreterResult {
        InterpreterResult::Error(InterpreterError::ProtocolNotImplemented(
            "As<string>",
            Self::NAME,
        ))
    }

    #[allow(unused_variables)]
    fn get(&self, interpreter: &mut Interpreter, _key: Arc<Object>) -> InterpreterResult {
        InterpreterResult::Error(InterpreterError::ProtocolNotImplemented("Get", Self::NAME))
    }

    fn to_object(self) -> Arc<Object> {
        Object::new(self).into()
    }
}

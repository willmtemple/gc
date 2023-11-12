use std::{
    any::TypeId,
    collections::hash_map::DefaultHasher,
    fmt::{self, Debug, Formatter},
    hash::{Hash, Hasher},
    sync::Arc,
};

use ::hamt::{vec::HamtVecSlice, HamtVec};

use crate::{Interpreter, InterpreterError, InterpreterResult, Value as Value1};

mod block;
mod function;
mod hamt;
mod native;
mod nil;
mod number;
mod primitive;
mod sigil;
mod string;
mod symbol;
mod tuple;

pub use block::Block;
pub use function::Function;
pub use hamt::{Map, Set, Vec};
pub use native::NativeObject;
pub use nil::Nil;
pub use sigil::Sigil;
pub use string::String;
pub use symbol::Symbol;
pub use tuple::Tuple;

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

    pub fn call(
        &self,
        interpreter: &mut Interpreter,
        args: HamtVecSlice<Arc<Object>>,
    ) -> InterpreterResult {
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

    pub fn to_value1(self: Arc<Self>) -> Value1 {
        if self.type_id == Legacy::TYPE_ID {
            self.downcast::<Legacy>().unwrap().value.clone()
        } else {
            Value1::Object(self)
        }
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

    pub egal: fn(*const (), &Object) -> bool,
    pub hash: fn(*const (), &mut dyn Hasher),

    pub call: fn(*const (), &mut Interpreter, HamtVecSlice<Arc<Object>>) -> InterpreterResult,
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
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: HamtVecSlice<Arc<Object>>,
    ) -> InterpreterResult {
        InterpreterResult::Error(InterpreterError::UncallableValue(Value1::Object(
            Nil.to_object(),
        )))
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

    fn to_value1(self) -> Value1 {
        Value1::Object(self.to_object())
    }
}

pub struct Legacy {
    value: Value1,
}

impl Legacy {
    pub fn new(value: Value1) -> Legacy {
        Legacy { value }
    }

    pub fn value(&self) -> &Value1 {
        &self.value
    }
}

impl Value for Legacy {
    const NAME: &'static str = "legacy";

    fn egal(&self, other: &Self) -> bool {
        self.value.eq(&other.value)
    }

    fn call(
        &self,
        _interpreter: &mut Interpreter,
        _args: HamtVecSlice<Arc<Object>>,
    ) -> InterpreterResult {
        self.value.call(
            _interpreter,
            _args
                .iter()
                .map(|v| Value1::Object(v.clone()))
                .collect::<HamtVec<_>>()
                .slice(..),
        )
    }

    fn get(&self, _interpreter: &mut Interpreter, _key: Arc<Object>) -> InterpreterResult {
        InterpreterResult::Value(self.value.get(_interpreter, &Value1::Object(_key)))
    }

    fn to_string(&self, _interpreter: &mut Interpreter) -> InterpreterResult {
        InterpreterResult::Value(String::from(self.value.print(_interpreter, 0)).to_value1())
    }

    fn d_hash(&self) -> InterpreterResult<u64> {
        let mut hasher = DefaultHasher::new();

        self.value.hash(&mut hasher);

        InterpreterResult::Value(hasher.finish())
    }
}

pub fn to_value1_args(s: HamtVecSlice<Arc<Object>>) -> HamtVecSlice<Value1> {
    s.iter()
        .map(|v| match v.type_name() {
            "legacy" => v.downcast::<Legacy>().unwrap().value().clone(),
            _ => Value1::Object(v.clone()),
        })
        .collect::<HamtVec<_>>()
        .slice(..)
}

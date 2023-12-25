use std::{any::TypeId, sync::Arc};

use hamt::{config::CloningConfig, vec::HamtVecSlice, HamtVec};

use crate::util::unordered_pair::UnorderedPair;

use super::{Object, Type, Value};

/// # Numeric Loosening
///
/// We define _many_ numeric types that are not generally instruction-compatible.
///
/// In order to resolve cases where we have different numeric types on either side of an operation, we use a process of
/// "loosening". This process ensures that numeric precision is maintained in every operation by progressively weakening
/// constraints of the numbers that can inhabit the result type.
///
/// The numeric types are arranged in a directed acyclic graph with `decimal` at the root. Each type is guaranteed to
/// losslessly convert to its parent type. When operating on different numeric types, the result is the type that is
/// the nearest common ancestor of the two types, to which they are losslessly converted before the operation is
/// computed.
///
/// ## Edges
///
/// The root is `rational`.
///
///   integer : rational
///   isize   : integer
///   i128    : integer
///   i64     : i128
///   i32     : i64, f64, isize
///   i16     : i32
///   i8      : i16
///   natural : integer
///   usize   : natural
///   u128    : natural
///   u64     : u128, i128
///   u32     : u64, i64, f64, usize
///   u16     : u32, i32
///   u8      : u16, i16
///   f64     : rational // what about NaN/infty? TODO/wtemple
///   f32     : f64 // f64 encodes f32 losslessly? TODO/wtemple
///
/// ## Examples
///
/// i64 + i32 = i64 (i32 is losslessly converted to i64)
/// i64 + u32 = i64 (u32 is losslessly converted to i64)
/// f32 + usize = decimal (both are losslessly converted to decimal)
///
mod _doc {}

macro_rules! numeric_table {
    { $($type:ty $(: $($parent:ty),*)?;)*} => {
        $(impl Value for $type {
            fn egal(&self, other: &Self) -> bool {
                self == other
            }

            fn to_string(&self, _: &mut crate::Interpreter) -> crate::InterpreterResult {
                crate::InterpreterResult::Value(
                    super::String::from(<$type as ToString>::to_string(self)).to_object(),
                )
            }
        })*
    }
}

type Digits = HamtVec<Digit, CloningConfig>;

#[cfg(target_pointer_width = "64")]
type Digit = u64;

#[cfg(target_pointer_width = "32")]
type Digit = u32;

#[cfg(target_pointer_width = "64")]
type SignedDigit = i64;

#[cfg(target_pointer_width = "32")]
type SignedDigit = i32;

#[cfg(target_pointer_width = "64")]
type TwiceDigit = u128;

#[cfg(target_pointer_width = "32")]
type TwiceDigit = u64;

#[derive(PartialEq, Eq, Hash, Clone)]
#[allow(non_camel_case_types)]
struct rational {
    value: integer,
    scale: i64,
}

impl core::fmt::Display for rational {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

#[derive(PartialEq, Eq, Hash, Clone, Copy)]
enum Sign {
    Positive,
    Negative,
}

#[derive(PartialEq, Eq, Hash, Clone)]
#[allow(non_camel_case_types)]
struct integer {
    sign: Sign,
    value: natural,
}

impl core::fmt::Display for integer {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.sign == Sign::Negative {
            write!(f, "-{}", self.value)
        } else {
            write!(f, "{}", self.value)
        }
    }
}

#[derive(PartialEq, Eq, Hash, Clone)]
#[allow(non_camel_case_types)]
struct natural {
    data: Digits,
}

impl core::fmt::Display for natural {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        todo!()
    }
}

impl natural {
    fn zero() -> Self {
        Self {
            data: Digits::new(),
        }
    }

    fn digits(&self) -> HamtVecSlice<Digit, CloningConfig> {
        self.data.as_slice()
    }
}

impl From<natural> for integer {
    fn from(value: natural) -> Self {
        Self {
            sign: Sign::Positive,
            value,
        }
    }
}

impl From<integer> for rational {
    fn from(value: integer) -> Self {
        Self { value, scale: 0 }
    }
}

numeric_table! {
    rational;
    integer: rational;
    isize: integer;
    i128: integer;
    i64: i128;
    i32: i64, f64, isize;
    i16: i32;
    i8: i16;
    natural: integer;
    usize: natural;
    u128: natural;
    u64: u128, i128;
    u32: u64, i64, f64, usize;
    u16: u32, i32;
    u8: u16, i16;
    f64: rational;
    f32: f64;
}

macro_rules! numeric_typeid_map {
    ($($a:ty, $b:ty : $ancestor:ty);+ $(;)?) => {
        let mut m = std::collections::BTreeMap::new();

        $(m.insert(UnorderedPair(std::any::TypeId::of::<$a>(), std::any::TypeId::of::<$b>()), std::any::TypeId::of::<$ancestor>());)*

        m
    };
}

lazy_static::lazy_static! {
    static ref NUMERIC_ANCESTORS: std::collections::BTreeMap<UnorderedPair<TypeId>, TypeId> = {
        numeric_typeid_map! {
            f32, f32: f32;
            f32, f64: f64;
            f32, i128: rational;
            f32, i16: f64;
            f32, i32: f64;
            f32, i64: rational;
            f32, i8: f64;
            f32, integer: rational;
            f32, isize: rational;
            f32, natural: rational;
            f32, rational: rational;
            f32, u128: rational;
            f32, u16: f64;
            f32, u32: f64;
            f32, u64: rational;
            f32, u8: f64;
            f32, usize: rational;
            f64, f64: f64;
            f64, i128: rational;
            f64, i16: f64;
            f64, i32: f64;
            f64, i64: rational;
            f64, i8: f64;
            f64, integer: rational;
            f64, isize: rational;
            f64, natural: rational;
            f64, rational: rational;
            f64, u128: rational;
            f64, u16: f64;
            f64, u32: f64;
            f64, u64: rational;
            f64, u8: f64;
            f64, usize: rational;
            i128, i128: i128;
            i128, i16: i128;
            i128, i32: i128;
            i128, i64: i128;
            i128, i8: i128;
            i128, integer: integer;
            i128, isize: integer;
            i128, natural: integer;
            i128, rational: rational;
            i128, u128: integer;
            i128, u16: i128;
            i128, u32: i128;
            i128, u64: i128;
            i128, u8: i128;
            i128, usize: integer;
            i16, i16: i16;
            i16, i32: i32;
            i16, i64: i64;
            i16, i8: i16;
            i16, integer: integer;
            i16, isize: isize;
            i16, natural: integer;
            i16, rational: rational;
            i16, u128: integer;
            i16, u16: i32;
            i16, u32: i64;
            i16, u64: i128;
            i16, u8: i16;
            i16, usize: integer;
            i32, i32: i32;
            i32, i64: i64;
            i32, i8: i32;
            i32, integer: integer;
            i32, isize: isize;
            i32, natural: integer;
            i32, rational: rational;
            i32, u128: integer;
            i32, u16: i32;
            i32, u32: i64;
            i32, u64: i128;
            i32, u8: i32;
            i32, usize: integer;
            i64, i64: i64;
            i64, i8: i64;
            i64, integer: integer;
            i64, isize: integer;
            i64, natural: integer;
            i64, rational: rational;
            i64, u128: integer;
            i64, u16: i64;
            i64, u32: i64;
            i64, u64: i128;
            i64, u8: i64;
            i64, usize: integer;
            i8, i8: i8;
            i8, integer: integer;
            i8, isize: isize;
            i8, natural: integer;
            i8, rational: rational;
            i8, u128: integer;
            i8, u16: i32;
            i8, u32: i64;
            i8, u64: i128;
            i8, u8: i16;
            i8, usize: integer;
            integer, integer: integer;
            integer, isize: integer;
            integer, natural: integer;
            integer, rational: rational;
            integer, u128: integer;
            integer, u16: integer;
            integer, u32: integer;
            integer, u64: integer;
            integer, u8: integer;
            integer, usize: integer;
            isize, isize: isize;
            isize, natural: integer;
            isize, rational: rational;
            isize, u128: integer;
            isize, u16: isize;
            isize, u32: integer;
            isize, u64: integer;
            isize, u8: isize;
            isize, usize: integer;
            natural, natural: natural;
            natural, rational: rational;
            natural, u128: natural;
            natural, u16: natural;
            natural, u32: natural;
            natural, u64: natural;
            natural, u8: natural;
            natural, usize: natural;
            rational, rational: rational;
            rational, u128: rational;
            rational, u16: rational;
            rational, u32: rational;
            rational, u64: rational;
            rational, u8: rational;
            rational, usize: rational;
            u128, u128: u128;
            u128, u16: u128;
            u128, u32: u128;
            u128, u64: u128;
            u128, u8: u128;
            u128, usize: natural;
            u16, u16: u16;
            u16, u32: u32;
            u16, u64: u64;
            u16, u8: u16;
            u16, usize: usize;
            u32, u32: u32;
            u32, u64: u64;
            u32, u8: u32;
            u32, usize: usize;
            u64, u64: u64;
            u64, u8: u64;
            u64, usize: natural;
            u8, u8: u8;
            u8, usize: usize;
            usize, usize: usize;
        }
    };
}

pub fn is_numeric(o: Arc<Object>) -> bool {
    let tid = o.get_type();

    tid == Type::of::<rational>()
        || tid == Type::of::<integer>()
        || tid == Type::of::<natural>()
        || tid == Type::of::<isize>()
        || tid == Type::of::<i128>()
        || tid == Type::of::<i64>()
        || tid == Type::of::<i32>()
        || tid == Type::of::<i16>()
        || tid == Type::of::<i8>()
        || tid == Type::of::<usize>()
        || tid == Type::of::<u128>()
        || tid == Type::of::<u64>()
        || tid == Type::of::<u32>()
        || tid == Type::of::<u16>()
        || tid == Type::of::<u8>()
        || tid == Type::of::<f64>()
        || tid == Type::of::<f32>()
}

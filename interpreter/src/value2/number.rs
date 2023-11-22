use hamt::{config::CloningConfig, HamtVec};

use super::Value;

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
/// The root is `decimal`.
///
///   int     : decimal
///   isize   : int
///   i128    : int
///   i64     : i128
///   i32     : i64, f64, isize
///   i16     : i32
///   i8      : i16
///   usize   : int
///   u128    : int
///   u64     : u128, i128
///   u32     : u64, i64, f64, usize
///   u16     : u32, i32
///   u8      : u16, i16
///   f64     : decimal
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
            const NAME: &'static str = stringify!($type);

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
    value: int,
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
struct int {
    sign: Sign,
    value: natural,
}

impl core::fmt::Display for int {
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
}

impl From<natural> for int {
    fn from(value: natural) -> Self {
        Self {
            sign: Sign::Positive,
            value,
        }
    }
}

impl From<int> for rational {
    fn from(value: int) -> Self {
        Self { value, scale: 0 }
    }
}

numeric_table! {
    rational;
    int: rational;
    isize: int;
    i128: int;
    i64: i128;
    i32: i64, f64, isize;
    i16: i32;
    i8: i16;
    natural: int;
    usize: natural;
    u128: natural;
    u64: u128, i128;
    u32: u64, i64, f64, usize;
    u16: u32, i32;
    u8: u16, i16;
    f64: rational;
    f32: f64;
}

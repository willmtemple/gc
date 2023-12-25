// -- PRIMITIVE TYPES ----------------------------------------------------------
let string = __sys.types.string;
let i64 = __sys.types.i64;
let symbol = __sys.types.symbol;
let sigil = __sys.types.sigil;
let type = __sys.types.type;

let precedence_classes = #{
    factor: 0,
    term: 1,
    compare: 2,
    logical: 3,
    is: 4,
    nil_coalesce: 5,
    assign: 6,
};

fn now(base) {
    __sys.now(base)
}

fn print(...values) {
    __sys.println(...values);
}

fn str(v) {
    __sys.str(v)
}

fn operator(subject, sigil_tokens, class_tokens, position_tokens) {
  let position_class = if position_tokens {
    str(position_tokens[0])
  } else {
    "infix"
  };

  __sys.bind_operator(
    subject,
    sigil_tokens[0],
    precedence_classes[class_tokens[0]],
    position_class,
  );
}

// #[operator(:, is, infix)]
// fn is(v, type) {
//     if __sys.type_of(type) != type {
//         panic("is: type must be a type");
//     }
// 
//     __sys.type_of(v) == type
// }

#[operator(+, factor, infix)]
fn add(left, right) {
    __sys.add(left, right)
}

#[operator(-, factor, infix)]
fn sub(left, right) {
    __sys.sub(left, right)
}

#[operator(-, factor, prefix)]
fn minus(value) {
    __sys.neg(value)
}

#[operator(*, term, infix)]
fn mul(left, right) {
    __sys.mul(left, right)
}

#[operator(/, term, infix)]
fn div(left, right) {
    __sys.div(left, right)
}

#[operator(%, term, infix)]
fn mod(left, right) {
    __sys.mod(left, right)
}

#[operator(**, term, infix)]
fn pow(left, right) {
    __sys.pow(left, right)
}

#[operator(>, compare, infix)]
fn gt(left, right) {
    __sys.gt(left, right)
}

#[operator(<, compare, infix)]
fn lt(left, right) {
    __sys.lt(left, right)
}

#[operator(>=, compare, infix)]
fn gte(left, right) {
    __sys.gte(left, right)
}

#[operator(<=, compare, infix)]
fn lte(left, right) {
    __sys.lte(left, right)
}

#[operator(==, compare, infix)]
fn eq(left, right) {
    __sys.eq(left, right)
}

#[operator(!=, compare, infix)]
fn neq(left, right) {
    !eq(left, right)
}

#[operator(||, logical, infix)]
fn or(left, right) {
    if left {
        left
    } else {
        right
    }
}

#[operator(&&, logical, infix)]
fn and(left, right) {
  if left {
    right
  } else {
    left
  }
}

#[operator(!, logical, prefix)]
fn not(value) {
    if value {
        false
    } else {
        true
    }
}

#[operator(??, nil_coalesce, infix)]
fn nil_coalesce(left, right) {
    if left == nil {
        right
    } else {
        left
    }
}

#[operator(*, factor, prefix)]
fn deref(value) {
    value.get()
}

fn load_module(path) {
    __sys.load_module(path)
}

#[keyword]
fn import(tokens) {
    load_module(tokens[0]);
}

/// Creates a new unbound symbol with the given name. The symbol is guaranteed
/// to be globally unique.
fn gensym(name) {
    __sys.gensym(name)
}

/// Produces the type of the given value.
fn type(value) {
    __sys.type_of(value)
}

/// Terminates the currently-executing thread with the given message.
fn panic(message) {
    __sys.panic(message)
}

/// Asserts that the given condition is true, and panics with the given message
/// if it is not.
fn assert(condition, message) {
    if !condition {
        panic(message)
    }
}

/// Produces the length of the given value (i.e. the number of elements).
///
/// This method is implemented for the following built-in types:
///   - `string`
///   - `vec`
///   - `map`
///   - `set`
///
/// # Panics
///
/// Panics if the given value does not implement the `length` built-in protocol.
fn size(v) {
    __sys.len(v)
}

/// Produces a slice of the given vector, starting at the given index [start]
/// (inclusive) and ending at the given index [end] (exclusive).
fn slice(v, start, end) {
    __sys.slice(v, start, end)
}

/// Concatenates the given arguments into strings and produces the result.
fn strcat(...values) {
    __sys.strcat(...values)
}

/// Produces a [Sequence] from the given vector.
fn seq(v) {
    if size(v) == 0 {
        nil
    } else {
        #{
            first: v[0],
            rest: fn() {
                seq(slice(v, 1, size(v)))
            }
        }
    }
}

/// # For loop
/// 
/// The `for` keyword is used to iterate over a sequence of values.
///
/// The syntax is as follows:
///
/// ```text
/// for <binding> in <expression> {
///     <body>
/// }
/// ```
/// 
/// The `<expression>` is evaluated and must implement the built-in `sequence`
/// protocol.
///
/// The `<binding>` is a symbol or destructuring pattern that will be bound to
/// each value in the sequence, for which the body will be executed with the
/// given binding.
///
/// The built-in loop control keywords `break` and `continue` can be used to
/// control the flow of the loop just as with a basic [loop].
#[keyword]
fn for(binding, in, ...rest) {
    let body = rest[size(rest) - 1];
    let expr = slice(rest, 0, size(rest) - 1);

    assert(type(body) == "block", "for loop body must be a block");

    assert(
        (type(in) == "symbol") && (str(in) == ":in"),
        "for loop must use 'in' keyword"
    );

    quote! {
        let _it = __sys.def_local(
            seq($(...expr))
        );

        loop {
            let __it = __sys.get_local(_it);
            
            if __it == nil {
                break;
            }

            let $binding = __it.first;

            $body;

            __sys.set_local(_it, __it.rest());
        }
    }
}

#[keyword]
fn local(name, eq, ...expr) {
    print(type(name), symbol);
    assert(type(name) == symbol, strcat("local name must be a symbol, found:", "not a symbol"));
    assert(
        (type(eq) == sigil) && (str(eq) == "="),
        "local must use '=' sigil"
    );

    quote! let $name = {
        let _value = $(...expr);

        let _local = __sys.def_local($(...expr));

        #{
            type: "local",
            get: fn() {
                __sys.get_local(_local)
            },
            set: fn(value) {
                __sys.set_local(_local, value)
            }
        }
    }
}

#[operator(=, assign, infix)]
fn assign(dest, value) {
    if (type(dest) == "map") && (dest.type == "local") {
        dest.set(value)
    } else {
        panic("assignment destination must be a local")
    }
}


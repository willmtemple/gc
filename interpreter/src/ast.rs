use std::{mem::replace, ops::Deref, sync::Arc};

use hamt::{HamtMap, HamtSet, HamtVec};
use seglisp::{Body, SegLisp, SegLispNode};

use crate::{
    scope::Scope,
    value2::{self, Block, Map, Object, Set, Sigil, Slice, Symbol, Tuple, Value as Value2},
    Interpreter, InterpreterError, InterpreterResult,
};

// const RESERVED_WORDS: phf::Set<&'static str> = phf::phf_set! {
//     "fn",
//     "let",
//     "if",
//     "else",
//     "true",
//     "false",
//     "nil",
// };

fn seglisp_token_to_value(interpreter: &mut Interpreter, token: &SegLispNode) -> Arc<Object> {
    match &token.value {
        // Block patterns.
        SegLisp::List {
            delimiters: ('{', '}') | (char::REPLACEMENT_CHARACTER, char::REPLACEMENT_CHARACTER),
            contents,
        } => seglisp_body_to_block(interpreter, contents).to_object(),
        // Vector pattern.
        SegLisp::List {
            delimiters: ('[', ']'),
            contents,
        } => seglisp_segments_to_vecvec(interpreter, contents).to_object(),
        // Tuple pattern.
        SegLisp::List {
            delimiters: ('(', ')'),
            contents,
        } => seglisp_body_to_tuple(interpreter, contents),
        SegLisp::List {
            delimiters,
            contents: _,
        } => unreachable!("invalid delimiter pattern: {:?}", delimiters),

        SegLisp::Symbol(s) => Symbol { name: (*s).into() }.to_object(),
        SegLisp::Sigil(s) => interpreter.intern_sigil(s).to_object(),
        SegLisp::Number(n) => n
            .parse::<i64>()
            .unwrap_or_else(|_| panic!("failed to parse i64: {}", n))
            .to_object(),
        SegLisp::String(s) => value2::String::from(s.as_ref()).to_object(),
    }
}

fn seglisp_token_to_value_alt(
    interpreter: &mut Interpreter,
    token: &SegLispNode,
) -> Option<Arc<Object>> {
    // We have already parsed a `#` token, and this indicates that we want to interpret the next data structure as
    // some sort of literal.
    match &token.value {
        // #{} - map literal
        SegLisp::List {
            delimiters: ('{', '}'),
            contents,
        } => Some(seglisp_body_to_map(interpreter, contents)),
        // #() - set literal
        SegLisp::List {
            delimiters: ('(', ')'),
            contents,
        } => Some(seglisp_body_to_set(interpreter, contents)),
        _ => None,
    }
}

fn seglisp_segments_to_vecvec(interpreter: &mut Interpreter, tokens: &Body) -> value2::Vec {
    let mut bv = value2::Vec::default();

    for segment in tokens.data.iter() {
        let mut sv = value2::Vec::default();

        let mut alt = false;
        for token in segment.data.iter() {
            if let SegLisp::Sigil("#") = token.value {
                alt = true;
                continue;
            }

            if alt {
                if let Some(v) = seglisp_token_to_value_alt(interpreter, token) {
                    sv = sv.push(v);
                } else {
                    sv = sv.push(interpreter.intern_sigil("#").to_object());
                    sv = sv.push(seglisp_token_to_value(interpreter, token));
                }
                alt = false;
            } else {
                sv = sv.push(seglisp_token_to_value(interpreter, token));
            }
        }

        bv = bv.push(sv.to_object());
    }

    bv
}

pub fn seglisp_body_to_block(interpreter: &mut Interpreter, tokens: &Body) -> Block {
    let body = seglisp_segments_to_vecvec(interpreter, tokens);

    Block { body }
}

fn seglisp_body_to_tuple(interpreter: &mut Interpreter, tokens: &Body) -> Arc<Object> {
    let bv = seglisp_segments_to_vecvec(interpreter, tokens);

    Tuple::new(bv).to_object()
}

fn seglisp_body_to_map(interpreter: &mut Interpreter, tokens: &Body) -> Arc<Object> {
    let mut m = Map::new();

    for (entry_idx, entry) in tokens.data.iter().enumerate() {
        // lhs:TOKEN ':' rhs:TOKEN+

        let mut idx = 0;

        let lhs = match entry.data.get(idx).map(|v| &v.value) {
            Some(SegLisp::Symbol(s)) => Symbol { name: (*s).into() }.to_object(),
            Some(SegLisp::String(s)) => value2::String::from(s.as_ref()).to_object(),
            Some(SegLisp::List {
                delimiters: ('(', ')'),
                contents,
            }) => seglisp_body_to_tuple(interpreter, contents),
            None if entry_idx == tokens.data.len() - 1 => {
                // This is the last entry in the map, and it is empty. This is allowed.
                break;
            }
            v => panic!(
                "expected first token in mapline to be a symbol or tuple, found {:?} with entry_idx {} of {}",
                v,
                entry_idx,
                tokens.data.len()
            ),
        };

        idx += 1;

        // expect :
        if let Some(SegLisp::Sigil(":")) = entry.data.get(idx).map(|x| &x.value) {
            idx += 1;
        } else {
            panic!("expected : after first token in mapline");
        }

        // Make sure there are still tokens remaining
        if entry.data.len() <= idx {
            panic!("expected token(s) after : in mapline");
        }

        let mut rhs = value2::Vec::default();

        for token in entry.data.iter().skip(idx) {
            rhs = rhs.push(seglisp_token_to_value(interpreter, token));
        }

        m = m.insert(lhs, rhs.to_object());
    }

    m.to_object()
}

fn seglisp_body_to_set(interpreter: &mut Interpreter, tokens: &Body) -> Arc<Object> {
    let bv = seglisp_segments_to_vecvec(interpreter, tokens);

    bv.iter().cloned().collect::<Set>().to_object()
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Expr {
    Quote(Slice),
    Fn {
        name: Option<Symbol>,
        params: HamtVec<Param>,
        body: Block,
    },
    Let {
        name: Symbol,
        value: Box<Expr>,
        r#in: Option<Box<Expr>>,
    },
    If {
        condition: Box<Expr>,
        then: Box<Expr>,
        r#else: Option<Box<Expr>>,
    },
    Map(HamtVec<(Expr, Expr)>),
    Set(HamtSet<Expr>),
    Vec(HamtVec<Expr>),
    Tuple(HamtVec<Expr>),
    Number(i64),
    String(Arc<str>),
    Symbol(Symbol),
    Boolean(bool),
    Name(Symbol),
    Block(Block),
    Access {
        base: Box<Expr>,
        key: ValueOrExpr,
    },
    Call {
        callee: ValueOrExpr,
        args: HamtVec<Argument>,
    },
    Return {
        value: Option<Box<Expr>>,
    },
    Loop {
        body: Block,
    },
    Break {
        value: Option<Box<Expr>>,
    },
    Continue,
    Nil,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Argument {
    Expr(Box<Expr>),
    Spread(Box<Expr>),
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum ValueOrExpr {
    Value(Arc<Object>),
    Expr(Box<Expr>),
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub struct Operator {
    pub precedence: i64,
    pub position: OperatorPosition,
    pub value: Arc<Object>,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone, Copy)]
pub enum OperatorPosition {
    Prefix,
    Infix,
    Postfix,
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Param {
    Symbol(Symbol),
    Rest(Symbol),
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(Arc<Object>),
    UnknownOperator(Sigil),
    MacroExpansionError(Box<InterpreterError>),
    FunctionEvaluationError(Box<InterpreterError>),
}

pub fn parse_exprs(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<HamtVec<Expr>, ParseError> {
    let mut bv = HamtVec::new();

    let mut can_continue = true;

    while can_continue {
        let e = parse_expr(interpreter, scope, stream)?;
        // We allow expressions that _require_ a block at the end to be followed by another expression without a
        // segmenter. TODO/wtemple - currently we only allow this for `if` and `fn`, but it may be possible to determine
        // this for keyword macros as well if they _always_ require a block. For example, the `atomic` macro I'm always
        // thinking of... it would always require a block, so we would want to allow it to be followed by another expr
        // without a semicolon between them.
        can_continue = !stream.is_empty()
            && matches!(e, Expr::Fn { .. } | Expr::If { .. } | Expr::Loop { .. });

        bv = bv.push(e);
    }

    // If the stream isn't empty...
    if !stream.is_empty() {
        eprintln!("2, {}", can_continue);
        Err(ParseError::UnexpectedToken(stream.next().unwrap()))
    } else {
        Ok(bv)
    }
}

pub fn parse_expr(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    let mut attrs = parse_attributes(interpreter, stream);

    stream.reset_peek();

    let e = match stream.peek() {
        Some(s) if s.is::<Symbol>() => {
            let s = s.downcast::<Symbol>().unwrap();

            match s.name.deref() {
                "fn" => {
                    let binding = if let Some(name) = stream.peek() {
                        if name.is::<Symbol>() {
                            Some(scope.define_with_attrs(
                                name.downcast::<Symbol>().unwrap(),
                                attrs.clone(),
                            ))
                        } else {
                            None
                        }
                    } else {
                        None
                    };

                    let expr = parse_function(interpreter, scope, stream)?;

                    let attrs = replace(&mut attrs, HamtMap::new());

                    if let Some(binding) = binding {
                        let fn_val = match interpreter.eval_expr(scope, &expr) {
                            InterpreterResult::Value(v) => v,
                            InterpreterResult::Control(c) => {
                                return Err(ParseError::FunctionEvaluationError(Box::new(
                                    InterpreterError::UnexpectedControlFlow(c),
                                )));
                            }
                            InterpreterResult::Error(e) => {
                                return Err(ParseError::FunctionEvaluationError(Box::new(e)));
                            }
                        };

                        for (name, attr) in attrs.iter() {
                            if let Some(attr_fn) = scope.resolve(name) {
                                attr_fn
                                    .call(interpreter, {
                                        let mut args = value2::Vec::default();

                                        args = args.push(fn_val.clone());

                                        for arg in attr.arguments.iter() {
                                            args = args.push(arg.clone());
                                        }

                                        args.as_slice()
                                    })
                                    .expect_result()
                                    .expect("failed to call attribute fn");
                            }
                        }

                        binding
                            .value
                            .set(fn_val)
                            .expect("failed to eager-bind parsed function");
                    }

                    expr
                }
                "let" => parse_let(stream, interpreter, scope)?,
                "if" => parse_if(stream, interpreter, scope)?,
                "quote!" => parse_quote(stream, interpreter, scope)?,
                "loop" => {
                    stream.next();

                    // Expect a block
                    let peek = stream.peek();

                    let Some(body) = peek.as_ref().and_then(|v| v.downcast::<Block>()).cloned()
                    else {
                        eprintln!("1");
                        return Err(ParseError::UnexpectedToken(stream.next().unwrap()));
                    };

                    stream.next();

                    Expr::Loop { body }
                }
                "break" => {
                    stream.next();

                    if stream.peek().is_some() {
                        stream.reset_peek();

                        let v = parse_element(interpreter, scope, stream)?;

                        Expr::Break {
                            value: Some(Box::new(v)),
                        }
                    } else {
                        Expr::Break { value: None }
                    }
                }
                "continue" => {
                    stream.next();
                    Expr::Continue
                }
                "return" => {
                    stream.next();

                    if stream.peek().is_some() {
                        stream.reset_peek();

                        let v = parse_element(interpreter, scope, stream)?;

                        Expr::Return {
                            value: Some(Box::new(v)),
                        }
                    } else {
                        Expr::Return { value: None }
                    }
                }
                _ if is_keyword_macro(interpreter, scope, s) => {
                    stream.next();
                    let macro_fn = scope.resolve(s).unwrap();
                    let tokens = stream.unwrap();
                    *stream = TokenStream::new(value2::Vec::default().as_slice());

                    let segment_value = match macro_fn.call(interpreter, tokens) {
                        InterpreterResult::Value(v) => v,
                        InterpreterResult::Control(c) => {
                            return Err(ParseError::MacroExpansionError(Box::new(
                                InterpreterError::UnexpectedControlFlow(c),
                            )));
                        }
                        InterpreterResult::Error(e) => {
                            return Err(ParseError::MacroExpansionError(Box::new(e)));
                        }
                    };

                    let Some(segment) = segment_value.downcast::<value2::Vec>() else {
                        panic!("Expected macro to return vec, returned {:?}", segment_value)
                    };

                    parse_expr(
                        interpreter,
                        scope,
                        &mut TokenStream::new(segment.as_slice()),
                    )?
                }
                _ => parse_term(interpreter, &mut scope.clone(), stream)?,
            }
        }
        // If we encounter a symbol that isn't a builtin, it's a lexically scoped name.
        // In this position, it could be a keyword macro. If it's a kw macro, we call the macro function with the rest
        // of the segment as the argument.
        Some(_) => parse_term(interpreter, &mut scope.clone(), stream)?,
        None => Expr::Nil,
    };

    Ok(e)
}

fn parse_if(
    stream: &mut TokenStream,
    interpreter: &mut Interpreter,
    scope: &mut Scope,
) -> Result<Expr, ParseError> {
    stream.next();
    let condition = Box::new(parse_term(interpreter, scope, stream)?);
    let then = Box::new(parse_term(interpreter, scope, stream)?);

    stream.reset_peek();

    let next = stream.peek();

    let r#else = if let Some(s) = next.as_ref().and_then(|v| v.downcast::<Symbol>()) {
        if s.name.deref() == "else" {
            stream.next();

            Some(Box::new(parse_term(interpreter, scope, stream)?))
        } else {
            None
        }
    } else {
        None
    };

    Ok(Expr::If {
        condition,
        then,
        r#else,
    })
}

fn parse_let(
    stream: &mut TokenStream,
    interpreter: &mut Interpreter,
    scope: &mut Scope,
) -> Result<Expr, ParseError> {
    stream.next();
    let next = stream.next();
    let Some(name) = next.as_ref().and_then(|v| v.downcast::<Symbol>()).cloned() else {
        panic!("expected name to be a symbol, found {:?}", next)
    };

    let next = stream.next();

    let is_eq = if let Some(s) = next.as_ref().and_then(|v| v.downcast::<Sigil>()) {
        s.text(interpreter) == "="
    } else {
        false
    };
    if !is_eq {
        panic!("expected = after name in binding")
    }
    let value = Box::new(parse_expr(interpreter, scope, stream)?);

    // optional `in` keyword
    let next = stream.peek();
    let r#in = if let Some(s) = next.as_ref().and_then(|v| v.downcast::<Symbol>()) {
        if s.name.deref() == "in" {
            stream.next();
            Some(Box::new(parse_expr(interpreter, scope, stream)?))
        } else {
            None
        }
    } else {
        None
    };

    Ok(Expr::Let { name, value, r#in })
}

fn parse_function(
    interpreter: &mut Interpreter,
    scope: &Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    stream.next();
    stream.reset_peek();
    let peek = stream.peek();
    let name = if let Some(s) = peek.as_ref().and_then(|v| v.downcast::<Symbol>()) {
        stream.next();
        Some(s.clone())
    } else {
        None
    };

    let next = stream.next();
    let Some(params) = next.as_ref().and_then(|v| v.downcast::<Tuple>()) else {
        panic!("expected params to be a vector")
    };
    let params = params
        .data()
        .iter()
        .map(|v| {
            if v.is::<value2::Vec>() {
                parse_param(
                    interpreter,
                    scope,
                    &mut TokenStream::new(v.downcast::<value2::Vec>().unwrap().as_slice()),
                )
            } else {
                panic!("expected all params to be within vectors")
            }
        })
        .collect::<Result<HamtVec<Param>, ParseError>>()?;

    // Check that "rest" params are only allowed in the last position.
    let mut rest = false;

    for param in params.iter() {
        if rest {
            panic!("a rest parameter argument must be in the last position")
        }

        match param {
            Param::Symbol(_) => {}
            Param::Rest(_) => {
                rest = true;
            }
        }
    }

    let next = stream.next();

    let Some(body) = next.as_ref().and_then(|v| v.downcast::<Block>()).cloned() else {
        panic!("expected body to be a block")
    };

    Ok(Expr::Fn { name, params, body })
}

#[derive(Debug, Clone, Eq, PartialEq, Hash)]
pub struct Attribute {
    pub name: Symbol,
    pub arguments: value2::Vec,
}

fn parse_attributes(
    interpreter: &Interpreter,
    tokens: &mut TokenStream,
) -> HamtMap<Symbol, Attribute> {
    let mut attrs = HamtMap::new();

    tokens.reset_peek();

    loop {
        let peek = tokens.peek();

        let Some(s) = peek.as_ref().and_then(|v| v.downcast::<Sigil>()) else {
            break;
        };

        if s.text(interpreter) == "#" {
            tokens.next();

            let next = tokens.next();

            let Some(attrs_vec) = next.as_ref().and_then(|v| v.downcast::<value2::Vec>()) else {
                panic!("expected vector after # in attributes")
            };

            if attrs_vec.len() == 0 {
                panic!("expected at least one attribute");
            }

            for attr in attrs_vec.iter() {
                let Some(attr) = attr.downcast::<value2::Vec>() else {
                    panic!("expected each individual attribute to be a vector")
                };

                let attr = parse_attribute(&mut TokenStream::new(attr.as_slice()));

                attrs = attrs.insert(attr.name.clone(), attr);
            }
        } else {
            break;
        }
    }

    attrs
}

fn parse_attribute(tokens: &mut TokenStream) -> Attribute {
    let next = tokens.next();
    let Some(name) = next.as_ref().and_then(|v| v.downcast::<Symbol>()).cloned() else {
        panic!("expected attribute name to be a symbol")
    };

    // Name may be followed by a tuple of arguments, which will become the attribute's args itself

    let next = tokens.next();

    let arguments = if let Some(args) = next.as_ref().and_then(|v| v.downcast::<Tuple>()) {
        args.data().clone()
    } else {
        value2::Vec::default()
    };

    // Check that the vec is empty
    if tokens.next().is_some() {
        panic!("expected attribute to end after arguments");
    }

    Attribute { name, arguments }
}

fn parse_param(
    interpreter: &mut Interpreter,
    _scope: &Scope,
    stream: &mut TokenStream,
) -> Result<Param, ParseError> {
    let first = stream.next().unwrap();

    if first.is::<Symbol>() {
        Ok(Param::Symbol(first.downcast::<Symbol>().unwrap().clone()))
    } else if first.is::<Sigil>() {
        let s = first.downcast::<Sigil>().unwrap();
        if s.text(interpreter) == "..." {
            let next = stream.next();
            let Some(s) = next.as_ref().and_then(|v| v.downcast::<Symbol>()).cloned() else {
                panic!("expected symbol after & in param")
            };

            Ok(Param::Rest(s))
        } else {
            eprintln!("3");
            Err(ParseError::UnexpectedToken(first))
        }
    } else {
        eprintln!("4");
        Err(ParseError::UnexpectedToken(first))
    }
}

fn parse_quote(
    stream: &mut TokenStream,
    _interpreter: &mut Interpreter,
    _scope: &mut Scope,
) -> Result<Expr, ParseError> {
    stream.next();

    Ok(Expr::Quote(stream.drain()))
}

// Outermost expr parsing stage
fn parse_term(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    stream.reset_peek();

    parse_infix(interpreter, scope, stream)
}

fn parse_infix(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    stream.reset_peek();

    let mut lhs = parse_prefix(interpreter, scope, stream)?;

    stream.reset_peek();

    // TODO/wtemple - operator precedence is important
    while !stream.is_empty() {
        let peek = stream.peek();

        if let Some(s) = peek.as_ref().and_then(|v| v.downcast::<Sigil>()).cloned() {
            stream.next();

            let Some(operator) = interpreter.get_operator(&s, OperatorPosition::Infix) else {
                return Err(ParseError::UnknownOperator(s));
            };

            if operator.position != OperatorPosition::Infix {
                eprintln!("5");
                return Err(ParseError::UnexpectedToken(s.to_object()));
            }

            let rhs = parse_infix(interpreter, scope, stream)?;

            lhs = Expr::Call {
                callee: ValueOrExpr::Value(operator.value),
                args: HamtVec::new()
                    .push(Argument::Expr(Box::new(lhs)))
                    .push(Argument::Expr(Box::new(rhs))),
            }
        } else {
            break;
        }
    }

    Ok(lhs)
}

fn parse_prefix(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    stream.reset_peek();

    let peek = stream.peek();

    if let Some(s) = peek.as_ref().and_then(|v| v.downcast::<Sigil>()) {
        if let Some(operator) = interpreter.get_operator(s, OperatorPosition::Prefix) {
            stream.next();

            let rhs = parse_prefix(interpreter, scope, stream)?;

            return Ok(Expr::Call {
                callee: ValueOrExpr::Value(operator.value),
                args: HamtVec::new().push(Argument::Expr(Box::new(rhs))),
            });
        }
    }

    parse_factor(interpreter, scope, stream)
}

// Parse tightly-bound operators like `.`, `[]` access, `()` calls, etc.
fn parse_factor(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    stream.reset_peek();

    let mut subject = parse_element(interpreter, scope, stream)?;

    stream.reset_peek();

    while !stream.is_empty() {
        stream.reset_peek();
        subject = match stream.peek() {
            Some(s) if s.downcast::<Sigil>().map(|s| s.text(interpreter) == ".") == Some(true) => {
                stream.next();

                let next = stream.next();

                let Some(s) = next.as_ref().and_then(|v| v.downcast::<Symbol>()).cloned() else {
                    panic!("expected symbol after .")
                };

                Expr::Access {
                    base: Box::new(subject),
                    key: ValueOrExpr::Value(s.to_object()),
                }
            }
            Some(v) if v.is::<value2::Vec>() => {
                let v = v.downcast::<value2::Vec>().unwrap();
                stream.next();

                if v.len() != 1 {
                    panic!("expected vector to have one element parsing index access")
                }

                let slice = v.as_slice();

                let Some(term_tokens) = slice.get(0).and_then(|v| v.downcast::<value2::Vec>())
                else {
                    panic!("fatal error: expected vec in vec")
                };

                let key = parse_term(
                    interpreter,
                    scope,
                    &mut TokenStream::new(term_tokens.as_slice()),
                )?;

                Expr::Access {
                    base: Box::new(subject),
                    key: ValueOrExpr::Expr(Box::new(key)),
                }
            }
            Some(args) if args.is::<Tuple>() => {
                let args = args.downcast::<Tuple>().unwrap();
                stream.next();

                let args = args
                    .data()
                    .iter()
                    .map(|v| {
                        if v.is::<value2::Vec>() {
                            parse_arg(
                                interpreter,
                                scope,
                                &mut TokenStream::new(
                                    v.downcast::<value2::Vec>().unwrap().as_slice(),
                                ),
                            )
                        } else {
                            panic!("expected all params to be within vectors")
                        }
                    })
                    .collect::<Result<HamtVec<Argument>, ParseError>>()?;

                Expr::Call {
                    callee: ValueOrExpr::Expr(Box::new(subject)),
                    args,
                }
            }
            _ => break,
        }
    }

    Ok(subject)
}

// Innermost expr parsing stage
fn parse_element(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    let first = stream.next().unwrap();

    let r = if first.is::<Map>() {
        Expr::Map({
            let m = first.downcast::<Map>().unwrap();
            let mut hm = HamtVec::new();
            for (k, v) in m.iter() {
                let k = if k.is::<Symbol>() {
                    Expr::Symbol(k.downcast::<Symbol>().unwrap().clone())
                } else if k.is::<value2::String>() {
                    Expr::String(
                        k.downcast::<value2::String>()
                            .map(|v| v.value.clone())
                            .unwrap(),
                    )
                } else if k.downcast::<Tuple>().map(|t| t.len() == 1) == Some(true) {
                    let t = k.downcast::<Tuple>().unwrap();
                    let t = t.data().as_slice();
                    let Some(v_tokens) = t.get(0).unwrap().downcast::<value2::Vec>() else {
                        panic!("expected vector in tuple");
                    };

                    parse_expr(
                        interpreter,
                        scope,
                        &mut TokenStream::new(v_tokens.as_slice()),
                    )?
                } else if k.is::<Tuple>() {
                    let t = k.downcast::<Tuple>().unwrap();

                    Expr::Tuple(
                        t.data()
                            .iter()
                            .map(|v| {
                                let Some(v_tokens) = v.downcast::<value2::Vec>() else {
                                    panic!("expected vector in map");
                                };

                                let v = parse_expr(
                                    interpreter,
                                    scope,
                                    &mut TokenStream::new(v_tokens.as_slice()),
                                )?;

                                Ok(v)
                            })
                            .collect::<Result<HamtVec<Expr>, ParseError>>()?,
                    )
                } else {
                    panic!(
                        "expected symbol, string, tuple, or parenthesized expression on left-hand side of map, but found {:?}", v
                    )
                };

                let Some(v_tokens) = v.downcast::<value2::Vec>() else {
                    panic!("expected vector in map");
                };

                let v = parse_expr(
                    interpreter,
                    scope,
                    &mut TokenStream::new(v_tokens.as_slice()),
                )?;

                hm = hm.push((k, v));
            }
            hm
        })
    } else if first.is::<Set>() {
        let s = first.downcast::<Set>().unwrap();
        let mut hs = HamtSet::new();
        for v in s.iter() {
            let Some(v_tokens) = v.downcast::<value2::Vec>() else {
                panic!("expected vector in set");
            };

            let v = parse_expr(
                interpreter,
                scope,
                &mut TokenStream::new(v_tokens.as_slice()),
            )?;

            hs = hs.insert(v);
        }

        Expr::Set(hs)
    } else if first.is::<value2::Vec>() {
        let v = first.downcast::<value2::Vec>().unwrap();
        let mut hv = HamtVec::new();
        for v in v.iter() {
            let Some(v_tokens) = v.downcast::<value2::Vec>() else {
                panic!("expected vector in vector");
            };

            let v = parse_expr(
                interpreter,
                scope,
                &mut TokenStream::new(v_tokens.as_slice()),
            )?;

            hv = hv.push(v);
        }

        Expr::Vec(hv)
    } else if first.downcast::<Tuple>().map(|t| t.len() <= 1) == Some(true) {
        let t = first.downcast::<Tuple>().unwrap();
        let contents = if let Some(v_tokens) = t
            .data()
            .as_slice()
            .get(0)
            .and_then(|v| v.downcast::<value2::Vec>())
        {
            v_tokens.clone()
        } else {
            value2::Vec::default()
        };

        parse_expr(
            interpreter,
            scope,
            &mut TokenStream::new(contents.as_slice()),
        )?
    } else if first.is::<Tuple>() {
        let t = first.downcast::<Tuple>().unwrap();
        let mut hv = HamtVec::new();
        for v in t.data().iter() {
            let Some(v_tokens) = v.downcast::<value2::Vec>() else {
                panic!("expected vector in tuple");
            };

            hv = hv.push(parse_expr(
                interpreter,
                scope,
                &mut TokenStream::new(v_tokens.as_slice()),
            )?);
        }

        Expr::Tuple(hv)
    } else if first.is::<i64>() {
        Expr::Number(*first.downcast::<i64>().unwrap())
    } else if first.is::<bool>() {
        Expr::Boolean(*first.downcast::<bool>().unwrap())
    } else if first.is::<value2::String>() {
        Expr::String(first.downcast::<value2::String>().unwrap().value.clone())
    } else if first.is::<Symbol>() {
        let s = first.downcast::<Symbol>().unwrap();

        match s.name.deref() {
            "true" => Expr::Boolean(true),
            "false" => Expr::Boolean(false),
            "nil" => Expr::Nil,
            _ => Expr::Name(s.clone()),
        }
    } else if first.is::<Block>() {
        Expr::Block(first.downcast::<Block>().unwrap().clone())
    } else if first.is::<Sigil>() {
        let s = first.downcast::<Sigil>().unwrap();

        unreachable!(
            "sigil '{}' should have been reached at higher parse stages",
            s.text(interpreter),
        )
    } else {
        unreachable!(
            "object of type '{}' cannot be constructed by lexer",
            first.type_name()
        )
    };

    Ok(r)
}

fn parse_arg(
    interpreter: &mut Interpreter,
    scope: &mut Scope,
    stream: &mut TokenStream,
) -> Result<Argument, ParseError> {
    match stream.peek() {
        Some(s) if s.downcast::<Sigil>().map(|s| s.text(interpreter) == "...") == Some(true) => {
            stream.next();

            Ok(Argument::Spread(Box::new(parse_expr(
                interpreter,
                scope,
                stream,
            )?)))
        }
        _ => Ok(Argument::Expr(Box::new(parse_expr(
            interpreter,
            scope,
            stream,
        )?))),
    }
}

fn is_keyword_macro(interpreter: &Interpreter, scope: &Scope, s: &Symbol) -> bool {
    // TODO/wtemple
    let kw_attr = interpreter
        .get_attribute("keyword")
        .expect("Interpreter has no 'keyword' attribute.");

    scope
        .get_binding(s)
        .is_some_and(|v| v.attributes.get(&kw_attr).is_some())
}

pub struct TokenStream {
    tokens: Slice,
    peek_idx: usize,
}

impl TokenStream {
    pub fn new(tokens: Slice) -> Self {
        Self {
            tokens,
            peek_idx: 0,
        }
    }

    pub fn unwrap(&self) -> Slice {
        self.tokens.clone()
    }

    pub fn drain(&mut self) -> Slice {
        let tokens = self.tokens.clone();
        self.tokens = value2::Vec::default().as_slice();
        self.peek_idx = 0;
        tokens
    }

    pub fn next(&mut self) -> Option<Arc<Object>> {
        if !self.tokens.is_empty() {
            let token = self.tokens.get(0).cloned();
            self.tokens = self.tokens.slice(1..);
            self.peek_idx = if self.peek_idx == 0 {
                0
            } else {
                self.peek_idx - 1
            };

            token
        } else {
            None
        }
    }

    pub fn peek(&mut self) -> Option<Arc<Object>> {
        if self.peek_idx >= self.tokens.len() {
            None
        } else {
            let token = self.tokens.get(self.peek_idx);
            self.peek_idx += 1;
            token.cloned()
        }
    }

    pub fn reset_peek(&mut self) {
        self.peek_idx = 0;
    }

    pub fn is_empty(&self) -> bool {
        self.tokens.is_empty()
    }
}

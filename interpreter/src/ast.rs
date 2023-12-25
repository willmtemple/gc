use std::{mem::replace, ops::Deref, sync::Arc};

use hamt::{HamtMap, HamtSet, HamtVec};

use crate::{
    scope::{InternedSymbol, Let, LexicalEnvironment, OldScope, Scope},
    value::{self, Block, LexicalBlock, Map, Object, Set, Sigil, Slice, Symbol, Tuple, Value},
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

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum Expr {
    Literal(Arc<Object>),
    Quote(Slice),
    Fn {
        name: Option<Symbol>,
        params: HamtVec<Parameter>,
        body: Block,
    },
    Let {
        name: InternedSymbol,
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
    Name(InternedSymbol),
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
        id: Option<Symbol>,
        body: Block,
    },
    Break {
        value: Option<Box<Expr>>,
    },
    Continue {
        id: Option<Symbol>,
    },
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
pub enum Parameter {
    Symbol(InternedSymbol),
    Rest(InternedSymbol),
}

#[derive(Eq, PartialEq, Hash, Debug, Clone)]
pub enum ParseError {
    UnexpectedToken(Arc<Object>),
    UnknownOperator(Sigil),
    MacroExpansionError(Box<InterpreterError>),
    FunctionEvaluationError(Box<InterpreterError>),
}

pub fn parse_block(
    interpreter: &mut Interpreter,
    lexical: &LexicalBlock,
    scope: Scope,
) -> Result<Block, ParseError> {
    let mut scope = scope;

    let mut body = HamtVec::new();

    for seg_vec in &lexical.body {
        let Some(segment) = seg_vec.downcast::<value::Vec>() else {
            panic!(
                "expected all elements of LexicalBlock to be vectors, but found {}",
                seg_vec.get_type().name()
            );
        };

        let mut tokens = TokenStream::new(segment.as_slice());

        for expr in parse_exprs(interpreter, &mut scope, &mut tokens)?
            .iter()
            .cloned()
        {
            body = body.push(expr);
        }
    }

    let environ = LexicalEnvironment::empty();

    Ok(Block::new(body, environ))
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
                                        let mut args = value::Vec::default();

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

                    Expr::Loop { id: None, body }
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
                    Expr::Continue { id: None }
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
                    *stream = TokenStream::new(value::Vec::default().as_slice());

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

                    let Some(segment) = segment_value.downcast::<value::Vec>() else {
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
    scope: &mut OldScope,
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
    scope: &mut OldScope,
) -> Result<Expr, ParseError> {
    stream.next();
    let next = stream.next();
    let Some(name) = next.as_ref().and_then(|v| v.downcast::<Symbol>()).cloned() else {
        panic!("expected name to be a symbol, found {:?}", next)
    };

    let binding = scope.define(name.clone(), Let::uninitialized(name.clone()));

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

    Ok(Expr::Let {
        binding,
        value,
        r#in,
    })
}

fn parse_function(
    interpreter: &mut Interpreter,
    scope: &OldScope,
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
            if v.is::<value::Vec>() {
                parse_param(
                    interpreter,
                    scope,
                    &mut TokenStream::new(v.downcast::<value::Vec>().unwrap().as_slice()),
                )
            } else {
                panic!("expected all params to be within vectors")
            }
        })
        .collect::<Result<HamtVec<Parameter>, ParseError>>()?;

    // Check that "rest" params are only allowed in the last position.
    let mut rest = false;

    for param in params.iter() {
        if rest {
            panic!("a rest parameter argument must be in the last position")
        }

        match param {
            Parameter::Symbol(_) => {}
            Parameter::Rest(_) => {
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
    pub arguments: value::Vec,
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

            let Some(attrs_vec) = next.as_ref().and_then(|v| v.downcast::<value::Vec>()) else {
                panic!("expected vector after # in attributes")
            };

            if attrs_vec.len() == 0 {
                panic!("expected at least one attribute");
            }

            for attr in attrs_vec.iter() {
                let Some(attr) = attr.downcast::<value::Vec>() else {
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
        value::Vec::default()
    };

    // Check that the vec is empty
    if tokens.next().is_some() {
        panic!("expected attribute to end after arguments");
    }

    Attribute { name, arguments }
}

fn parse_param(
    interpreter: &mut Interpreter,
    _scope: &OldScope,
    stream: &mut TokenStream,
) -> Result<Parameter, ParseError> {
    let first = stream.next().unwrap();

    if first.is::<Symbol>() {
        Ok(Parameter::Symbol(
            first.downcast::<Symbol>().unwrap().clone(),
        ))
    } else if first.is::<Sigil>() {
        let s = first.downcast::<Sigil>().unwrap();
        if s.text(interpreter) == "..." {
            let next = stream.next();
            let Some(s) = next.as_ref().and_then(|v| v.downcast::<Symbol>()).cloned() else {
                panic!("expected symbol after & in param")
            };

            Ok(Parameter::Rest(s))
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
    _scope: &mut OldScope,
) -> Result<Expr, ParseError> {
    stream.next();

    Ok(Expr::Quote(stream.drain()))
}

// Outermost expr parsing stage
fn parse_term(
    interpreter: &mut Interpreter,
    scope: &mut OldScope,
    stream: &mut TokenStream,
) -> Result<Expr, ParseError> {
    stream.reset_peek();

    parse_infix(interpreter, scope, stream)
}

fn parse_infix(
    interpreter: &mut Interpreter,
    scope: &mut OldScope,
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
    scope: &mut OldScope,
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
            Some(v) if v.is::<value::Vec>() => {
                let v = v.downcast::<value::Vec>().unwrap();
                stream.next();

                if v.len() != 1 {
                    panic!("expected vector to have one element parsing index access")
                }

                let slice = v.as_slice();

                let Some(term_tokens) = slice.get(0).and_then(|v| v.downcast::<value::Vec>())
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
                        if v.is::<value::Vec>() {
                            parse_arg(
                                interpreter,
                                scope,
                                &mut TokenStream::new(
                                    v.downcast::<value::Vec>().unwrap().as_slice(),
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
                let k = if k.is::<Symbol>() || k.is::<value::String>() {
                    Expr::Literal(k.clone())
                } else if k.downcast::<Tuple>().map(|t| t.len() == 1) == Some(true) {
                    let t = k.downcast::<Tuple>().unwrap();
                    let t = t.data().as_slice();
                    let Some(v_tokens) = t.get(0).unwrap().downcast::<value::Vec>() else {
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
                                let Some(v_tokens) = v.downcast::<value::Vec>() else {
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

                let Some(v_tokens) = v.downcast::<value::Vec>() else {
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
            let Some(v_tokens) = v.downcast::<value::Vec>() else {
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
    } else if first.is::<value::Vec>() {
        let v = first.downcast::<value::Vec>().unwrap();
        let mut hv = HamtVec::new();
        for v in v.iter() {
            let Some(v_tokens) = v.downcast::<value::Vec>() else {
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
            .and_then(|v| v.downcast::<value::Vec>())
        {
            v_tokens.clone()
        } else {
            value::Vec::default()
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
            let Some(v_tokens) = v.downcast::<value::Vec>() else {
                panic!("expected vector in tuple");
            };

            hv = hv.push(parse_expr(
                interpreter,
                scope,
                &mut TokenStream::new(v_tokens.as_slice()),
            )?);
        }

        Expr::Tuple(hv)
    } else if first.is::<i64>() || first.is::<bool>() || first.is::<value::String>() {
        Expr::Literal(first.clone())
    } else if first.is::<Symbol>() {
        let s = first.downcast::<Symbol>().unwrap();

        match s.name.deref() {
            "true" => Expr::Literal(true.to_object()),
            "false" => Expr::Literal(false.to_object()),
            "nil" => Expr::Literal(().to_object()),
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
            first.get_type().name()
        )
    };

    Ok(r)
}

fn parse_arg(
    interpreter: &mut Interpreter,
    scope: &mut OldScope,
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

fn is_keyword_macro(interpreter: &Interpreter, scope: &OldScope, s: &Symbol) -> bool {
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
        self.tokens = value::Vec::default().as_slice();
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

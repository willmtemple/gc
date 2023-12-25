use std::sync::Arc;

use seglisp::{Body, SegLisp, SegLispNode};

use crate::{
    value::{self, LexicalBlock, Map, Object, Set, Symbol, Tuple, Value},
    Interpreter,
};

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
        SegLisp::String(s) => Arc::<str>::from(s.as_ref()).to_object(),
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

fn seglisp_segments_to_vecvec(interpreter: &mut Interpreter, tokens: &Body) -> value::Vec {
    let mut bv = value::Vec::default();

    for segment in tokens.data.iter() {
        let mut sv = value::Vec::default();

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

pub fn seglisp_body_to_block(interpreter: &mut Interpreter, tokens: &Body) -> LexicalBlock {
    let body = seglisp_segments_to_vecvec(interpreter, tokens);

    LexicalBlock { body }
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
            Some(SegLisp::String(s)) => Arc::<str>::from(s.as_ref()).to_object(),
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

        let mut rhs = value::Vec::default();

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

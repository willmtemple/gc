#![feature(try_trait_v2)]
#![feature(let_chains)]
#![feature(const_type_id)]
#![feature(const_type_name)]

use std::{
    convert::Infallible,
    io::stdout,
    ops::{Deref, FromResidual},
    path::{Path, PathBuf},
    process::exit,
    sync::Arc,
    time::{SystemTime, UNIX_EPOCH},
};

enum ExitCode {
    Panic = 1,
}

use ast::{Expr, Operator, OperatorPosition, ParseError, TokenStream};
use hamt::{HamtMap, HamtVec};
use list::List;
use read::seglisp_body_to_block;
use scope::{Let, LexicalEnvironment, OldScope};
use seglisp::{
    diagnostics::format_and_write_diagnostics_with_termcolor, read_str, ReaderConfiguration,
};
use value::{Block, Function, Map, NativeObject, Object, Set, Sigil, Slice, Symbol, Tuple, Value};

use crate::ast::{parse_expr, ValueOrExpr};

mod ast;
mod builtins;
mod list;
mod locals;
pub mod protocol;
pub mod read;
mod scope;
mod stm;
mod util;
pub mod value;

pub enum WriteTarget {
    Out,
    Err,
}

#[derive(Debug)]
pub struct CallFrame {
    module: Arc<Path>,
}

pub trait InterpreterHost {
    fn write_std(&mut self, target: WriteTarget, s: &str);
    fn load_boot_module(&mut self) -> Option<Arc<Path>>;
    fn resolve_module(&mut self, module_name: &str) -> Option<Arc<Path>>;
    fn read_file(&mut self, path: &Path) -> Option<Arc<str>>;
}

pub struct Interpreter {
    host: Box<dyn InterpreterHost>,
    call_stack: List<CallFrame>,
    boot_scope: OldScope,
    operators: HamtMap<(Sigil, OperatorPosition), Operator>,
    sigils: Sigils,
}

struct Sigils {
    sigils: HamtMap<Arc<str>, Sigil>,
    sigil_texts: HamtVec<Arc<str>>,
}

impl Default for Sigils {
    fn default() -> Self {
        Self {
            sigils: HamtMap::new(),
            sigil_texts: HamtVec::new(),
        }
    }
}

impl Sigils {
    fn intern(&mut self, sigil: impl AsRef<str>) -> Sigil {
        let sigil_name = Arc::<str>::from(sigil.as_ref());

        let cur = self.sigils.get(&sigil_name);

        if let Some(sigil) = cur {
            *sigil
        } else {
            let sigil = Sigil {
                identity: self.sigil_texts.len(),
            };

            self.sigils = self.sigils.insert(sigil_name.clone(), sigil);

            self.sigil_texts = self.sigil_texts.push(sigil_name);

            sigil
        }
    }

    fn get_text(&self, sigil: &Sigil) -> &str {
        self.sigil_texts.get(sigil.identity).unwrap()
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum InterpreterError {
    FailedToLoadInitModule,
    ModuleNotFound(PathBuf),
    ReadError,
    ParseError(ParseError),
    UnknownName(Arc<str>),
    Rebind(Symbol),
    UncallableValue(Arc<Object>),
    UnexpectedControlFlow(ControlFlow),
    NotIndexable(Arc<Object>),
    InvalidIndex(Arc<Object>),
    ProtocolNotImplemented(&'static str, &'static str),
    InvalidArity(usize, usize),
    UnexpectedType {
        expected: &'static str,
        actual: &'static str,
    },
    ArgumentError {
        expected: &'static str,
        position: usize,
    },
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum InterpreterResult<V = Arc<Object>> {
    Value(V),
    Control(ControlFlow),
    Error(InterpreterError),
}

impl<V> From<Result<V, InterpreterError>> for InterpreterResult<V> {
    fn from(r: Result<V, InterpreterError>) -> Self {
        match r {
            Ok(v) => Self::Value(v),
            Err(e) => Self::Error(e),
        }
    }
}

impl<V> InterpreterResult<V> {
    pub fn expect_result(self) -> Result<V, InterpreterError> {
        match self {
            Self::Value(v) => Ok(v),
            Self::Control(c) => Err(InterpreterError::UnexpectedControlFlow(c)),
            Self::Error(e) => Err(e),
        }
    }

    pub fn map_err(self, f: impl FnOnce(InterpreterError) -> InterpreterError) -> Self {
        match self {
            Self::Value(v) => Self::Value(v),
            Self::Control(c) => Self::Control(c),
            Self::Error(e) => Self::Error(f(e)),
        }
    }
}

impl<V> core::ops::Try for InterpreterResult<V> {
    type Output = V;
    type Residual = Self;

    fn branch(self) -> std::ops::ControlFlow<Self::Residual, Self::Output> {
        match self {
            Self::Value(v) => std::ops::ControlFlow::Continue(v),
            Self::Control(_) => std::ops::ControlFlow::Break(self),
            Self::Error(_) => std::ops::ControlFlow::Break(self),
        }
    }

    fn from_output(output: Self::Output) -> Self {
        Self::Value(output)
    }
}

impl<T, V> FromResidual<InterpreterResult<T>> for InterpreterResult<V> {
    fn from_residual(residual: InterpreterResult<T>) -> Self {
        match residual {
            InterpreterResult::Value(_) => unreachable!(),
            InterpreterResult::Control(c) => Self::Control(c),
            InterpreterResult::Error(e) => Self::Error(e),
        }
    }
}

impl<V> FromResidual<Result<Infallible, InterpreterError>> for InterpreterResult<V> {
    fn from_residual(residual: Result<Infallible, InterpreterError>) -> Self {
        match residual {
            Ok(_) => unreachable!(),
            Err(e) => Self::Error(e),
        }
    }
}

impl<V1, V2> FromIterator<InterpreterResult<V1>> for InterpreterResult<V2>
where
    V2: FromIterator<V1>,
{
    fn from_iter<T: IntoIterator<Item = InterpreterResult<V1>>>(iter: T) -> Self {
        let mut iter = iter.into_iter();

        let mut values = Vec::new();

        loop {
            match iter.next() {
                Some(InterpreterResult::Value(v)) => values.push(v),
                Some(InterpreterResult::Control(c)) => return Self::Control(c),
                Some(InterpreterResult::Error(e)) => return Self::Error(e),
                None => break,
            }
        }

        Self::Value(values.into_iter().collect())
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Hash)]
pub enum ControlFlow {
    Return(Arc<Object>),
    Break(Arc<Object>),
    Continue,
}

impl Interpreter {
    pub fn new(host: Box<dyn InterpreterHost>) -> Self {
        let mut boot_scope = OldScope::new();

        let mut intermediate_interpreter = Self {
            host,
            call_stack: List::Nil,
            boot_scope: boot_scope.clone(),
            operators: HamtMap::new(),
            sigils: Default::default(),
        };

        intermediate_interpreter
            .run_boot_file(&mut boot_scope)
            .unwrap_or_else(|e| {
                eprintln!("FATAL ERROR: failed to run boot script: {:?}", e);
                std::process::exit(1);
            });

        let Interpreter {
            host,
            operators,
            sigils,
            ..
        } = intermediate_interpreter;

        Self {
            host,
            call_stack: List::Nil,
            boot_scope,
            operators,
            sigils,
        }
    }

    pub fn get_attribute(&self, name: &str) -> Option<Symbol> {
        Some(Symbol { name: name.into() })
    }

    pub fn get_operator(&self, sigil: &Sigil, position: OperatorPosition) -> Option<Operator> {
        self.operators.get(&(*sigil, position)).cloned()
    }

    pub fn intern_sigil(&mut self, text: impl AsRef<str>) -> Sigil {
        self.sigils.intern(text)
    }

    pub fn get_sigil_text(&self, sigil: &Sigil) -> &str {
        self.sigils.get_text(sigil)
    }

    fn get_current_module(&self) -> &Path {
        self.call_stack
            .first()
            .expect("fatal error: no module on call stack")
            .module
            .deref()
    }

    fn run_boot_file(&mut self, scope: &mut OldScope) -> Result<Arc<Object>, InterpreterError> {
        let config = ReaderConfiguration::default();

        let module_path = self.host.load_boot_module();

        let text = module_path
            .clone()
            .and_then(|m| self.host.read_file(m.deref()))
            .ok_or(InterpreterError::FailedToLoadInitModule)?;

        let result = read_str(&config, &text);

        if result.had_error {
            // TODO: hardcoded stdout
            format_and_write_diagnostics_with_termcolor(
                stdout(),
                result.diagnostics.iter(),
                Some(&text),
            )
            .expect("fatal error: failed to write diagnostics");

            Err(InterpreterError::ReadError)
        } else {
            let module = &result.document;

            let module = seglisp_body_to_block(self, module);

            let mut sys = builtins::get_builtins();

            sys = sys.insert(
                Symbol::new("gensym").to_object(),
                NativeObject::new()
                    .with_call(|_, arguments| {
                        let Some(prefix) = arguments
                            .get(3)
                            .and_then(|v| v.downcast::<Arc<str>>())
                            .map(|v| v.deref())
                        else {
                            panic!("First argument to gensym should be a string.")
                        };

                        InterpreterResult::Value(Symbol::new(prefix).to_object())
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("get_type_name").to_object(),
                NativeObject::new()
                    .with_call(|_, arguments| {
                        if arguments.len() != 1 {
                            panic!("get_type_name expects exactly one argument.");
                        }

                        let value = arguments.get(0).unwrap();

                        eprintln!("get_type_name: {:?}", value.get_type().name());

                        InterpreterResult::Value(
                            Arc::<str>::from(value.get_type().name()).to_object(),
                        )
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("panic").to_object(),
                NativeObject::new()
                    .with_call(|_, arguments| {
                        if arguments.len() != 1 {
                            panic!("panic expects exactly one argument.");
                        }

                        let Some(message) = arguments.get(0).and_then(|v| v.downcast::<Arc<str>>())
                        else {
                            panic!("Argument to panic must be a string.")
                        };

                        eprintln!("Panic.");
                        eprintln!("{}", message);

                        exit(ExitCode::Panic as i32);
                    })
                    .to_object(),
            );

            // fn len(data: _[]): number
            sys = sys.insert(
                Symbol::new("len").to_object(),
                NativeObject::new()
                    .with_call(|_, args| {
                        if args.len() != 1 {
                            panic!("len expects exactly one argument.");
                        }

                        let data = args.get(0).unwrap();

                        let Some(data) = data.downcast::<value::Vec>() else {
                            panic!("Argument to len must be an array.")
                        };

                        InterpreterResult::Value((data.len() as i64).to_object())
                    })
                    .to_object(),
            );

            // fn slice[T](data: T[], start: number, end: number): T[]
            sys = sys.insert(
                Symbol::new("slice").to_object(),
                NativeObject::new()
                    .with_call(|_, args| {
                        if args.len() != 3 {
                            panic!("slice expects exactly three arguments.");
                        }

                        let data = args.get(0).unwrap();
                        let start = args.get(1).unwrap();
                        let end = args.get(2).unwrap();

                        let Some(data) = data.downcast::<value::Vec>() else {
                            panic!("First argument to slice must be an array.")
                        };

                        let Some(start) = start.downcast::<i64>() else {
                            panic!("Second argument to slice must be a number.")
                        };

                        let Some(end) = end.downcast::<i64>() else {
                            panic!("Third argument to slice must be a number.")
                        };

                        InterpreterResult::Value(
                            data.slice((*start as usize)..(*end as usize)).to_object(),
                        )
                    })
                    .to_object(),
            );

            // fn strcat(...args: string[]): string
            sys = sys.insert(
                Symbol::new("strcat").to_object(),
                NativeObject::new()
                    .with_call(|_, args| {
                        let mut result = String::new();

                        for arg in &args {
                            let Some(arg) = arg.downcast::<Arc<str>>() else {
                                panic!("Arguments to strcat must be strings.")
                            };

                            result.push_str(arg.deref());
                        }

                        InterpreterResult::Value(Arc::<str>::from(result).to_object())
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("def_local").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, args| {
                        InterpreterResult::Value(locals::def_local(interpreter, args))
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("get_local").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, args| {
                        InterpreterResult::Value(locals::get_local(interpreter, args))
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("set_local").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, args| {
                        InterpreterResult::Value(locals::set_local(interpreter, args))
                    })
                    .to_object(),
            );

            // fn str(v: _): string;
            sys = sys.insert(
                Symbol::new("str").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, args| {
                        if args.len() != 1 {
                            panic!("str expects exactly one argument.");
                        }

                        let value = args.get(0).unwrap();

                        InterpreterResult::Value(value.to_string(interpreter)?)
                    })
                    .to_object(),
            );

            // fn time(base?: number): number;
            sys = sys.insert(
                Symbol::new("now").to_object(),
                NativeObject::new()
                    .with_call(|_, args| {
                        let base = args
                            .get(0)
                            .and_then(|v| v.downcast::<i64>().copied())
                            .unwrap_or(0) as u128;

                        let now = SystemTime::now()
                            .duration_since(UNIX_EPOCH)
                            .unwrap()
                            .as_nanos();

                        InterpreterResult::Value(((now - base) as i64).to_object())
                    })
                    .to_object(),
            );

            let __sys = Symbol::new("__sys");

            scope.define(__sys.clone(), Let::with_value(__sys, sys.to_object()));

            let __tokens = Symbol::new("__tokens");

            scope.define(
                __tokens.clone(),
                Let::with_value(__tokens, module.clone().to_object()),
            );

            self.call_stack = self.call_stack.push_start(CallFrame {
                module: module_path.unwrap().clone(),
            });

            let r = self.eval_block(scope, &module);

            self.call_stack = self
                .call_stack
                .rest()
                .expect("fatal error: call stack underflow")
                .clone();

            r.expect_result()
        }
    }

    pub fn read_and_eval_module(
        &mut self,
        path: impl AsRef<Path>,
    ) -> Result<Arc<Object>, InterpreterError> {
        let config = ReaderConfiguration::default();

        let text = self
            .host
            .read_file(path.as_ref())
            .ok_or(InterpreterError::ModuleNotFound(PathBuf::from(
                path.as_ref(),
            )))?;
        let text = text.deref();

        let result = read_str(&config, text);

        if result.had_error {
            // TODO: hardcoded stdout
            format_and_write_diagnostics_with_termcolor(
                stdout(),
                result.diagnostics.iter(),
                Some(text),
            )
            .expect("fatal error: failed to write diagnostics");

            Err(InterpreterError::ReadError)
        } else {
            let module = &result.document;

            let module = seglisp_body_to_block(self, module);

            self.call_stack = self.call_stack.push_start(CallFrame {
                module: Arc::from(path.as_ref()),
            });

            let r = self.eval_block(&mut self.boot_scope.clone(), &module);

            self.call_stack = self
                .call_stack
                .rest()
                .expect("call stack corruption: underflow after eval_block")
                .clone();

            r.expect_result()
        }
    }

    fn eval_expr(&mut self, expr: &Expr, env: LexicalEnvironment) -> InterpreterResult {
        let v = match expr {
            Expr::Quote(tokens) => quote_substitute(self, tokens.clone(), env)?.to_object(),
            Expr::Fn { name, params, body } => Function {
                name: name.as_ref().map(|v| v.name.clone()),
                params: params.clone(),
                body: body.clone(),
            }
            .to_object(),
            Expr::Let { name, value, r#in } => {
                let Some(binding) = env.get_binding(name) else {
                    panic!("no binding for '{}' in eval_expr", name.name());
                };

                binding.set(self.eval_expr(expr, env)?);

                if let Some(body) = r#in {
                    self.eval_expr(body, env)?
                } else {
                    ().to_object()
                }
            }
            Expr::If {
                condition,
                then,
                r#else,
            } => {
                let condition = self.eval_expr(condition, env)?;

                let cr = if condition.is::<()>() {
                    false
                } else if condition.is::<bool>() {
                    *condition.downcast::<bool>().unwrap()
                } else {
                    true
                };

                if cr {
                    self.eval_expr(then, env)?
                } else if let Some(r#else) = r#else {
                    self.eval_expr(r#else, env)?
                } else {
                    ().to_object()
                }
            }
            Expr::Map(m) => m
                .iter()
                .map(|(k, v)| -> InterpreterResult<(Arc<Object>, Arc<Object>)> {
                    InterpreterResult::Value((self.eval_expr(k, env)?, self.eval_expr(v, env)?))
                })
                .collect::<InterpreterResult<Map>>()?
                .to_object(),

            Expr::Set(s) => s
                .iter()
                .map(|v| InterpreterResult::Value(self.eval_expr(&mut scope.clone(), v)?))
                .collect::<InterpreterResult<Set>>()?
                .to_object(),
            Expr::Vec(v) => v
                .iter()
                .map(|v| InterpreterResult::Value(self.eval_expr(&mut scope.clone(), v)?))
                .collect::<InterpreterResult<value::Vec>>()?
                .to_object(),
            Expr::Tuple(t) => Tuple::new(
                t.iter()
                    .map(|v| InterpreterResult::Value(self.eval_expr(&mut scope.clone(), v)?))
                    .collect::<InterpreterResult<value::Vec>>()?,
            )
            .to_object(),
            Expr::Literal(v) => v.clone(),
            Expr::Name(n) => scope
                .resolve(n)
                .map(InterpreterResult::Value)
                .unwrap_or(InterpreterResult::Error(InterpreterError::UnknownName(
                    n.clone(),
                )))?
                .clone(),
            Expr::Block(blk) => self.eval_block(&mut scope.clone(), blk)?,
            Expr::Access { base, key } => {
                let base = self.eval_expr(scope, base)?;

                let key = match key {
                    ast::ValueOrExpr::Value(n) => n.clone(),
                    ast::ValueOrExpr::Expr(e) => self.eval_expr(scope, e)?,
                };

                base.get(self, key)?
            }
            Expr::Call { callee, args } => {
                let callee = match callee {
                    ValueOrExpr::Expr(callee) => self.eval_expr(scope, callee)?,
                    ValueOrExpr::Value(callee) => callee.clone(),
                };

                let args = args
                    .iter()
                    .flat_map(|arg| match arg {
                        ast::Argument::Expr(e) => {
                            vec![self.eval_expr(scope, e)]
                        }
                        ast::Argument::Spread(s) => {
                            let v = self.eval_expr(scope, s);

                            let InterpreterResult::Value(v) = v else {
                                return vec![v];
                            };

                            if v.is::<value::Vec>() {
                                v.downcast::<value::Vec>()
                                    .unwrap()
                                    .iter()
                                    .cloned()
                                    .map(InterpreterResult::Value)
                                    .collect()
                            } else if v.is::<Tuple>() {
                                let v = v.downcast::<Tuple>().unwrap();
                                v.data()
                                    .iter()
                                    .cloned()
                                    .map(InterpreterResult::Value)
                                    .collect()
                            } else {
                                vec![InterpreterResult::Error(InterpreterError::NotIndexable(v))]
                            }
                        }
                    })
                    .collect::<InterpreterResult<value::Vec>>()?;

                callee.call(self, args.as_slice())?
            }
            Expr::Loop { id: _id, body } => {
                let mut loop_scope = scope.clone();

                loop {
                    let block_result = self.eval_block(&mut loop_scope, body);

                    match block_result {
                        InterpreterResult::Control(ControlFlow::Break(v)) => break v,
                        InterpreterResult::Control(ControlFlow::Continue) => continue,
                        InterpreterResult::Value(_) => {}
                        InterpreterResult::Control(ControlFlow::Return(_)) => return block_result,
                        InterpreterResult::Error(_) => return block_result,
                    }
                }
            }
            Expr::Break { value } => {
                return InterpreterResult::Control(ControlFlow::Break({
                    if let Some(v) = value {
                        self.eval_expr(scope, v)?
                    } else {
                        ().to_object()
                    }
                }))
            }
            Expr::Continue { id: _id } => {
                return InterpreterResult::Control(ControlFlow::Continue);
            }
            Expr::Return { value } => {
                return InterpreterResult::Control(ControlFlow::Return({
                    if let Some(v) = value {
                        self.eval_expr(scope, v)?
                    } else {
                        ().to_object()
                    }
                }))
            }
        };

        InterpreterResult::Value(v)
    }

    pub fn eval_block(&mut self, block: &Block) -> InterpreterResult {
        let mut result = ().to_object();

        let (mut scope, parsed) = block.parsed(self, scope)?;

        eprintln!("Dumping scope:");
        scope.dump();

        for expr in &parsed {
            result = self.eval_expr(&mut scope, expr)?;
        }

        InterpreterResult::Value(result)
    }
}

fn quote_substitute(
    interpreter: &mut Interpreter,
    tokens: Slice,
    env: LexicalEnvironment,
) -> InterpreterResult<value::Vec> {
    let mut result = value::Vec::default();

    let mut stream = TokenStream::new(tokens);

    loop {
        match stream.next() {
            Some(s) if s.downcast::<Sigil>().map(|v| v.text(interpreter) == "$") == Some(true) => {
                // interpolate value from scope

                match stream.next() {
                    Some(name) if name.is::<Symbol>() => {
                        // let name = name.downcast::<Symbol>().unwrap();
                        // let value = scope
                        //     .resolve(name)
                        //     .unwrap_or_else(|| panic!("unbound symbol: {}", name.name))
                        //     .clone();

                        // result = result.push(value);
                        unimplemented!("dynamic name binding in lexical environment")
                    }
                    Some(t) if t.downcast::<Tuple>().map(|t| t.len() == 1) == Some(true) => {
                        let t = t.downcast::<Tuple>().unwrap();
                        let t = t.data().as_slice();
                        let Some(v) = t.get(0).and_then(|v| v.downcast::<value::Vec>()) else {
                            panic!("expected tuple to contain a vec")
                        };

                        let mut stream = TokenStream::new(v.as_slice());

                        let peek = stream.peek();

                        let should_spread =
                            if let Some(s) = peek.as_ref().and_then(|v| v.downcast::<Sigil>()) {
                                if s.text(interpreter) == "..." {
                                    stream.next();
                                    true
                                } else {
                                    false
                                }
                            } else {
                                false
                            };

                        let expr = parse_expr(interpreter, &mut scope.clone(), &mut stream)
                            .map_err(InterpreterError::ParseError)?;

                        let value = interpreter.eval_expr(&mut scope.clone(), &expr)?;

                        if should_spread {
                            let Some(v) = value.downcast::<value::Vec>() else {
                                panic!("expected value to be a vec")
                            };

                            for value in v {
                                result = result.push(value.clone());
                            }
                        } else {
                            result = result.push(value);
                        }
                    }
                    _ => panic!("expected symbol or parenthesized expression after $ in quote"),
                }
            }
            Some(b) if b.is::<Block>() => {
                let b = b.downcast::<Block>().unwrap();
                result = result.push(
                    Block::new(quote_substitute(interpreter, scope, b.body.as_slice())?)
                        .to_object(),
                );
            }
            Some(m) if m.is::<Map>() => {
                let m = m.downcast::<Map>().unwrap();
                let mut hm = Map::new();

                for (k, v) in m.iter() {
                    let k = quote_substitute(
                        interpreter,
                        scope,
                        value::Vec::default().push(k.clone()).as_slice(),
                    )?;
                    let k = k.as_slice();
                    let k = k.get(0).unwrap();

                    let Some(v) = v.downcast::<value::Vec>() else {
                        panic!("Expected value-side of mapline to be a vec.");
                    };

                    hm = hm.insert(
                        k.clone(),
                        quote_substitute(interpreter, scope, v.as_slice())?.to_object(),
                    );
                }

                result = result.push(hm.to_object());
            }
            Some(s) if s.is::<Set>() => todo!(),
            Some(v) if v.is::<value::Vec>() => {
                let v = v.downcast::<value::Vec>().unwrap();
                result =
                    result.push(quote_substitute(interpreter, scope, v.as_slice())?.to_object())
            }
            Some(t) if t.is::<Tuple>() => {
                let t = t.downcast::<Tuple>().unwrap();
                result = result.push(
                    Tuple::new(quote_substitute(interpreter, scope, t.data().as_slice())?)
                        .to_object(),
                );
            }
            Some(v) => {
                // push value

                result = result.push(v);
            }
            None => break,
        }
    }

    InterpreterResult::Value(result)
}

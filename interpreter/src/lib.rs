#![feature(try_trait_v2)]
#![feature(let_chains)]
#![feature(const_type_id)]

use std::{
    convert::Infallible,
    io::stdout,
    ops::{Deref, FromResidual},
    path::{Path, PathBuf},
    process::exit,
    sync::Arc,
};

enum ExitCode {
    Panic = 1,
}

use ast::{
    parse_exprs, seglisp_body_to_block, Expr, Operator, OperatorPosition, ParseError, TokenStream,
};
use hamt::{config::CloningConfig, HamtMap, HamtVec};
use list::List;
use scope::Scope;
use seglisp::{
    diagnostics::format_and_write_diagnostics_with_termcolor, read_str, ReaderConfiguration,
};
use value2::{
    Block, Function, Map, NativeObject, Nil, Object, Set, Sigil, Slice, Symbol, Tuple,
    Value as Value2,
};

use crate::ast::{parse_expr, ValueOrExpr};

mod ast;
mod list;
mod locals;
mod scope;
mod stm;
pub mod value2;

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
    boot_scope: Scope,
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
        let mut boot_scope = Scope::new();

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

    fn run_boot_file(&mut self, scope: &mut Scope) -> Result<Arc<Object>, InterpreterError> {
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

            let mut sys = value2::Map::new_with_config(CloningConfig::default());

            sys = sys.insert(
                Symbol::new("println").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, arguments| {
                        let mut init = false;
                        for argument in &arguments {
                            if init {
                                interpreter.host.write_std(WriteTarget::Out, " ");
                            } else {
                                init = true;
                            }

                            let s = argument.to_string(interpreter)?;

                            let s = s.to_string(interpreter)?;

                            interpreter.host.write_std(
                                WriteTarget::Out,
                                &s.downcast::<value2::String>().unwrap().value,
                            );
                        }

                        interpreter.host.write_std(WriteTarget::Out, "\n");

                        InterpreterResult::Value(Nil.to_object())
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("bind_operator").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, arguments| {
                        let Some(f) = arguments.get(0) else {
                            panic!("First argument to bind_operator should be an object.")
                        };

                        let Some(op) = arguments.get(1).and_then(|v| v.downcast::<Sigil>()) else {
                            panic!("Second argument to bind_operator should be a sigil.")
                        };

                        let Some(precedence) = arguments.get(2).and_then(|v| v.downcast::<i64>())
                        else {
                            panic!("Third argument to bind_operator should be a number.")
                        };

                        let Some(position) = arguments
                            .get(3)
                            .and_then(|v| v.downcast::<value2::String>())
                            .map(|v| v.value.deref())
                        else {
                            panic!("Fourth argument to bind_operator should be a string.")
                        };

                        let position = match position {
                            ":prefix" => OperatorPosition::Prefix,
                            ":infix" => OperatorPosition::Infix,
                            ":postfix" => OperatorPosition::Postfix,
                            _ => panic!("Invalid operator position: {}", position),
                        };

                        interpreter.operators = interpreter.operators.insert(
                            (*op, position),
                            Operator {
                                position,
                                precedence: *precedence,
                                value: f.clone(),
                            },
                        );

                        InterpreterResult::Value(Nil.to_object())
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("egal").to_object(),
                NativeObject::new()
                    .with_call(|_, arguments| {
                        let a = arguments.get(0).cloned().unwrap_or(Nil.to_object());
                        let b = arguments.get(1).cloned().unwrap_or(Nil.to_object());

                        InterpreterResult::Value((a == b).to_object())
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("load_module").to_object(),
                NativeObject::new()
                    .with_call(|interpreter, arguments| {
                        let Some(path) = arguments
                            .get(3)
                            .and_then(|v| v.downcast::<value2::String>())
                            .map(|v| v.value.deref())
                        else {
                            panic!("First argument to load_module should be a string.")
                        };

                        // We have a few cases. This works like Node.js module resolution.
                        // If the path doesn't start with `.` or `/`, we try to resolve it as a module using the host.
                        // If the path is absolute, we just load it from the host.
                        // If the path is definitely relative (i.e. starts with `.`), we load it relative to the current module's dirname.

                        let is_path = path.starts_with('.') || path.starts_with('/');

                        let final_path = if is_path {
                            let path = Path::new(path);

                            if path.is_absolute() {
                                PathBuf::from(path)
                            } else {
                                let module = interpreter.get_current_module();

                                module
                                    .parent()
                                    .expect("fatal error: module has no parent")
                                    .join(path)
                            }
                        } else {
                            let Some(module) = interpreter.host.resolve_module(path) else {
                                panic!("failed to resolve module '{}'", path);
                            };

                            PathBuf::from(module.deref())
                        };

                        InterpreterResult::from(interpreter.read_and_eval_module(final_path))
                    })
                    .to_object(),
            );

            sys = sys.insert(
                Symbol::new("gensym").to_object(),
                NativeObject::new()
                    .with_call(|_, arguments| {
                        let Some(prefix) = arguments
                            .get(3)
                            .and_then(|v| v.downcast::<value2::String>())
                            .map(|v| v.value.deref())
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

                        InterpreterResult::Value(
                            value2::String::from(value.type_name()).to_object(),
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

                        let Some(message) = arguments
                            .get(0)
                            .and_then(|v| v.downcast::<value2::String>())
                        else {
                            panic!("Argument to panic must be a string.")
                        };

                        eprintln!("Panic.");
                        eprintln!("{}", message.value);

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

                        let Some(data) = data.downcast::<value2::Vec>() else {
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

                        let Some(data) = data.downcast::<value2::Vec>() else {
                            panic!("First argument to slice must be an array.")
                        };

                        let Some(start) = start.downcast::<i64>() else {
                            panic!("Second argument to slice must be a number.")
                        };

                        let Some(end) = end.downcast::<i64>() else {
                            panic!("Third argument to slice must be a number.")
                        };

                        let data = data.slice((*start as usize)..(*end as usize));

                        InterpreterResult::Value(
                            data.iter().cloned().collect::<value2::Vec>().to_object(),
                        )
                    })
                    .to_object(),
            );

            // fn add(l: number, r: number): number
            sys = sys.insert(
                Symbol::new("add").to_object(),
                NativeObject::new()
                    .with_call(|_, args| {
                        if args.len() != 2 {
                            panic!("add expects exactly two arguments.");
                        }

                        let l = args.get(0).unwrap();
                        let r = args.get(1).unwrap();

                        let Some(l) = l.downcast::<i64>() else {
                            panic!("First argument to add must be a number.")
                        };

                        let Some(r) = r.downcast::<i64>() else {
                            panic!("Second argument to add must be a number.")
                        };

                        InterpreterResult::Value((*l + *r).to_object())
                    })
                    .to_object(),
            );

            // fn sub(l: number, r: number): number
            sys = sys.insert(
                Symbol::new("sub").to_object(),
                NativeObject::new()
                    .with_call(|_, args| {
                        if args.len() != 2 {
                            panic!("sub expects exactly two arguments.");
                        }

                        let l = args.get(0).unwrap();
                        let r = args.get(1).unwrap();

                        let Some(l) = l.downcast::<i64>() else {
                            panic!("First argument to sub must be a number.")
                        };

                        let Some(r) = r.downcast::<i64>() else {
                            panic!("Second argument to sub must be a number.")
                        };

                        InterpreterResult::Value((*l - *r).to_object())
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

            scope
                .define(&Symbol::new("__sys"))
                .value
                .set(sys.to_object())
                .expect("failed to bind __sys metaprotocol object");

            // scope
            //     .define(&Symbol::new("_p"))
            //     .value
            //     .set(
            //         NativeObject::new()
            //             .with_call(|interpreter, args| {
            //                 for arg in &args {
            //                     eprintln!(
            //                         "Arg: {}",
            //                         arg.to_string(interpreter)
            //                             .expect_result()
            //                             .unwrap()
            //                             .downcast::<value2::String>()
            //                             .unwrap()
            //                             .value
            //                     );
            //                 }

            //                 InterpreterResult::Value(Nil.to_object())
            //             })
            //             .to_object(),
            //     )
            //     .expect("failed to set _p");

            scope
                .define(&Symbol::new("__tokens"))
                .value
                .set(module.clone().to_object())
                .expect("fatal error: __tokens binding is already set, somehow");

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

    fn eval_expr(&mut self, scope: &mut Scope, expr: &Expr) -> InterpreterResult {
        let v = match expr {
            Expr::Quote(tokens) => quote_substitute(self, scope, tokens.clone())?.to_object(),
            Expr::Fn { name, params, body } => Function {
                name: name.as_ref().map(|v| v.name.clone()),
                params: params.clone(),
                body: body.clone(),
                scope: scope.clone(),
            }
            .to_object(),
            Expr::Let { name, value, r#in } => {
                let v = self.eval_expr(scope, value)?;

                scope.define(name).value.set(v.clone()).unwrap_or_else(|_| {
                    panic!("failed to bind '{}': value already set", name.name)
                });

                if let Some(body) = r#in {
                    self.eval_expr(scope, body)?
                } else {
                    Nil.to_object()
                }
            }
            Expr::If {
                condition,
                then,
                r#else,
            } => {
                let condition = self.eval_expr(scope, condition)?;

                let cr = if condition.is::<Nil>() {
                    false
                } else if condition.is::<bool>() {
                    *condition.downcast::<bool>().unwrap()
                } else {
                    true
                };

                if cr {
                    self.eval_expr(scope, then)?
                } else if let Some(r#else) = r#else {
                    self.eval_expr(scope, r#else)?
                } else {
                    Nil.to_object()
                }
            }
            Expr::Map(m) => m
                .iter()
                .map(|(k, v)| -> InterpreterResult<(Arc<Object>, Arc<Object>)> {
                    InterpreterResult::Value((
                        self.eval_expr(&mut scope.clone(), k)?,
                        self.eval_expr(&mut scope.clone(), v)?,
                    ))
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
                .collect::<InterpreterResult<value2::Vec>>()?
                .to_object(),
            Expr::Tuple(t) => Tuple::new(
                t.iter()
                    .map(|v| InterpreterResult::Value(self.eval_expr(&mut scope.clone(), v)?))
                    .collect::<InterpreterResult<value2::Vec>>()?,
            )
            .to_object(),
            Expr::Number(n) => n.to_object(),
            Expr::Boolean(b) => b.to_object(),
            Expr::String(s) => value2::String::from(s.clone()).to_object(),
            Expr::Symbol(s) => s.clone().to_object(),
            Expr::Name(n) => scope
                .resolve(n)
                .unwrap_or_else(|| {
                    panic!("unbound name {}", n.name);
                })
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

                            if v.is::<value2::Vec>() {
                                v.downcast::<value2::Vec>()
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
                    .collect::<InterpreterResult<value2::Vec>>()?;

                callee.call(self, args.as_slice())?
            }
            Expr::Loop { body } => {
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
                        Nil.to_object()
                    }
                }))
            }
            Expr::Continue => {
                return InterpreterResult::Control(ControlFlow::Continue);
            }
            Expr::Return { value } => {
                return InterpreterResult::Control(ControlFlow::Return({
                    if let Some(v) = value {
                        self.eval_expr(scope, v)?
                    } else {
                        Nil.to_object()
                    }
                }))
            }
            Expr::Nil => Nil.to_object(),
        };

        InterpreterResult::Value(v)
    }

    pub fn eval_block(&mut self, scope: &mut Scope, block: &Block) -> InterpreterResult {
        let Block { body } = block;

        let mut result = Nil.to_object();

        for segment in body {
            let Some(expr) = segment.downcast::<value2::Vec>() else {
                unreachable!("segment should always parse as a vec")
            };

            let exprs = parse_exprs(self, scope, &mut TokenStream::new(expr.as_slice()))
                .map_err(InterpreterError::ParseError)?;

            for expr in &exprs {
                result = self.eval_expr(scope, expr)?;
            }
        }

        InterpreterResult::Value(result)
    }
}

fn quote_substitute(
    interpreter: &mut Interpreter,
    scope: &Scope,
    tokens: Slice,
) -> InterpreterResult<value2::Vec> {
    let mut result = value2::Vec::default();

    let mut stream = TokenStream::new(tokens);

    loop {
        match stream.next() {
            Some(s) if s.downcast::<Sigil>().map(|v| v.text(interpreter) == "$") == Some(true) => {
                // interpolate value from scope

                match stream.next() {
                    Some(name) if name.is::<Symbol>() => {
                        let name = name.downcast::<Symbol>().unwrap();
                        let value = scope
                            .resolve(name)
                            .unwrap_or_else(|| panic!("unbound symbol: {}", name.name))
                            .clone();

                        result = result.push(value);
                    }
                    Some(t) if t.downcast::<Tuple>().map(|t| t.len() == 1) == Some(true) => {
                        let t = t.downcast::<Tuple>().unwrap();
                        let t = t.data().as_slice();
                        let Some(v) = t.get(0).and_then(|v| v.downcast::<value2::Vec>()) else {
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
                            let Some(v) = value.downcast::<value2::Vec>() else {
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
                    Block {
                        body: quote_substitute(interpreter, scope, b.body.as_slice())?,
                    }
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
                        value2::Vec::default().push(k.clone()).as_slice(),
                    )?;
                    let k = k.as_slice();
                    let k = k.get(0).unwrap();

                    let Some(v) = v.downcast::<value2::Vec>() else {
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
            Some(v) if v.is::<value2::Vec>() => {
                let v = v.downcast::<value2::Vec>().unwrap();
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

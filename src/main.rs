#![allow(warnings)]

use chumsky::{prelude::*, Span};
use once_cell::unsync::Lazy;
mod expr; use expr::*;
mod val; use val::*;
#[macro_use] mod state;
mod parser; use parser::*;
mod request; use request::*;

#[macro_export]
macro_rules! err {
  ($span:expr, $k:ident $($data:tt)?) => {
    Exception { traceback: vec![$span], data: ExceptionData::$k $($data)? }
  }
}

#[macro_export]
macro_rules! throw {
  ($span:expr, $k:ident $($data:tt)?) => {
    Err(err!($span, $k $($data)?))?
  }
}

#[macro_export]
macro_rules! coerce {
  ($span:expr, $val:expr => $ty:ident) => {
    match $val.type_of() {
      ValType::$ty => $val,
      _ => throw!($span, TypeError { expected: ValType::$ty, found: $val.type_of() })
    }
  }
}

static mut COLORGEN: Lazy<ColorGenerator> = Lazy::new(|| ColorGenerator::new());

#[macro_export]
macro_rules! default {
  () => { std::default::Default::default() }
}

fn boxed<T>(value: T) -> Box<T> {
  Box::new(value)
}

trait WithSpan<S: Span, T> where T: From<(S, Self)>, Self: Sized {
  fn with_span(self, span: S) -> T;
}

impl<S: Span, F, T> WithSpan<S, T> for F where T: From<(S, F)> {
  fn with_span(self, span: S) -> T {
    (span, self).into()
  }
}

use std::{collections::{HashMap, HashSet}, iter::zip, fmt::Display, cell::{RefCell, Ref, RefMut}, str::FromStr, num::{ParseFloatError}, ops::Range, process::ExitCode};

#[derive(Debug, Default)]
pub struct Closure<'a> {
  parent: Option<&'a Closure<'a>>,
  vars: RefCell<HashMap<String, Val>>
}

impl<'a> Closure<'a> {
  fn new() -> Self {
    Self { parent: None, vars: RefCell::new(HashMap::new()) }
  }
}
impl<'b, 'a: 'b> Closure<'a> {
  fn extend(&'a self, vars: HashMap<String, Val>) -> Closure<'b> {
    Self { parent: Some(self), vars: RefCell::new(vars) }
  }
}

impl<'a> Closure<'a> {
  fn find_owner<'b>(&'a self, var: &'b str) -> Option<&'a Closure<'a>> {
    if let Some(_) = self.vars().get(var) {
      Some(self)
    } else if let Some(parent) = self.parent {
      parent.find_owner(var)
    } else {
      None
    }
  }
  fn lookup(&self, var: &'a str) -> Option<Val> {
    let vars = self.vars();
    if let Some(val) = vars.get(var) {
      Some(val).cloned()
    } else if let Some(parent) = self.parent {
      parent.lookup(var)
    } else {
      None
    }
  }
  fn vars(&self) -> Ref<HashMap<String, Val>> {
    self.vars.borrow()
  }
  fn vars_mut(&self) -> RefMut<HashMap<String, Val>> {
    self.vars.borrow_mut()
  }
}

// struct Frame(String, Range<usize>);

#[derive(Clone, Debug)]
pub enum ExceptionData {
  TypeError { expected: ValType, found: ValType },
  ArgError { expected: usize, found: usize },
  VarError(String),
  NumError(ParseFloatError),
  ThrownError(Box<Expr>),
  AssignError,
  EvlError
}
impl ExceptionData {
  pub fn code(&self) -> u16 {
    match self {
      ExceptionData::TypeError { expected, found } => 3,
      ExceptionData::ArgError { expected, found } => 4,
      ExceptionData::VarError(_) => 5,
      ExceptionData::NumError(_) => 6,
      ExceptionData::ThrownError(_) => 7,
      ExceptionData::AssignError => 8,
      ExceptionData::EvlError => 9,
    }
  }
}

#[derive(Clone, Debug)]
pub struct Exception {
  traceback: Vec<Origin>,
  data: ExceptionData,
  // traceback: Vec<Frame>
}

impl Display for Exception {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
      write!(f, "Exception: {}", self.data)
    }
}

impl Display for ExceptionData {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      ExceptionData::TypeError { expected, found } => write!(f, "Incorrect type (expected {expected:?}, found {found:?})"),
      ExceptionData::ArgError { expected, found } => write!(f, "Incorrect argument count (expected {expected} parameters, found {found} arguments)"),
      ExceptionData::VarError(var) => write!(f, "Could not find variable {var}"),
      ExceptionData::NumError(err) => write!(f, "Number format error: {err}"),
      ExceptionData::ThrownError(err) => write!(f, "{:?}", err),
      ExceptionData::AssignError => write!(f, "Cannot assign to literal"),
      ExceptionData::EvlError => write!(f, "Syntax error in ~ operator"),
    }
  }
}

pub trait AddFrame: Sized {
  fn add_frame(&mut self, frame: Origin);
  fn with_frame(mut self, frame: Origin) -> Self {
    self.add_frame(frame);
    self
  }
}

impl AddFrame for Exception {
  fn add_frame(&mut self, frame: Origin) {
    self.traceback.push(frame);
  }
}

impl<T> AddFrame for Result<T, Exception> {
  fn add_frame(&mut self, frame: Origin) {
    if let Err(e) = self {
      e.add_frame(frame);
    }
  }
}

#[macro_export]
macro_rules! extract {
  ($output:expr, $input:expr, $pat:pat) => {
    match $input {
      $pat => Some($output),
      _ => None
    }
  };
}

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source, ReportBuilder};

#[macro_export]
macro_rules! option_match {
  ($pat:pat = $expr:expr => $val:expr) => {
    match $expr {
      $pat => Some($val),
      _ => None
    }
  };
}

#[macro_export]
macro_rules! match_or_err {
  ($pat:pat = $expr:expr => $val:expr) => {
    match $expr {
      $pat => Ok($val),
      val => Err(val)
    }
  };
}

impl From<(Origin, ParseFloatError)> for Exception {
  fn from(value: (Origin, ParseFloatError)) -> Self {
    Exception { data: ExceptionData::NumError(value.1), traceback: vec![value.0] }
  }
}

fn with_span<F: WithSpan<S, T>, T: From<(S, F)>, S: Span>(span: S) -> impl Fn(F) -> T {
  move |value| value.with_span(span.clone())
}

fn run_thread<F: Future>(future: F) -> F::Output {
  use std::task::{*, Poll::*};
  /// stolen from Waker::noop() — what's unstable about this?
  pub fn waker_noop() -> Waker {
    const VTABLE: RawWakerVTable = RawWakerVTable::new(
      // Cloning just returns a new no-op raw waker
      |_| RAW,
      // `wake` does nothing
      |_| {},
      // `wake_by_ref` does nothing
      |_| {},
      // Dropping does nothing as we don't allocate anything
      |_| {},
    );
    const RAW: RawWaker = RawWaker::new(std::ptr::null(), &VTABLE);

    unsafe { Waker::from_raw(RAW) }
  }
  let waker = waker_noop();
  let mut waker = Context::from_waker(&waker);
  match pin!(future).poll(&mut waker) {
    Ready(value) => value,
    Waiting => todo!("yielding threads not implemented"),
  }
}

fn main() -> ExitCode {
  let filename = "test.txt";
  let src = std::fs::read_to_string(filename).expect("Example file should be readable");
  println!("parsing");
  match parser(filename).parse(src.clone()) {
    Ok(exprs) => {
      let closure = Closure::new();
      for expr in exprs {
        match run_thread(expr.eval(&closure)) {
          Ok(val) => println!("{}", val),
          Err(Exception { data, traceback }) => {
            let mut report = Report::build(ReportKind::Error, filename, 0)
            .with_message(format!("{}", data))
            .with_code(data.code());
            for (i, loc) in traceback.into_iter().enumerate() {
              report.add_label(Label::new(loc.clone()).with_color(unsafe { COLORGEN.next() }).with_order(i as i32).with_priority(-(i as i32)).with_message(format!("at {}: {}..{}", loc.0, loc.1.start(), loc.1.end())));
            }
            report.finish().print((filename.to_owned(), Source::from(src.clone()))).expect("Failed to print exception");
            return 1.into()
          }
        }
      }
    },
    Err(errors) => {
      let mut report = Report::build(ReportKind::Error, filename, 0)
        .with_message("Invalid syntax");
      for err in errors {
        // Safety: there is no multithreading in this program
        let mut expected: HashSet<Option<char>> = err.expected().copied().collect();
        let allow_eof = expected.remove(&None);
        let expected: String = expected.into_iter().map(Option::unwrap).collect();
        let help = match (expected.len(), allow_eof) {
          (0, false) => "no characters are allowed here".to_owned(),
          (_, false) => format!("expected '{}'", expected.fg(unsafe { COLORGEN.next() })),
          (0, true) => format!("expected end of input"),
          (_, true) => format!("expected '{}' or end of input", expected.fg(unsafe { COLORGEN.next() }))
        };
        let label_color = unsafe { COLORGEN.next() };
        report = report
          .with_label(
            Label::new((filename, err.span()))
              .with_color(label_color)
              .with_message(format!(
                "Unexpected {} ({help})",
                match err.found() {
                  None => "end of input".to_string(),
                  Some(c) => format!("'{c}'").fg(label_color).to_string()
                }
              ))
          );
      }
      report.finish().print((filename, Source::from(src))).expect("Failed to print parse failure");
      return 2.into()
    }
  }
  return 0.into()
}
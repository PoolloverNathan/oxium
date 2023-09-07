use chumsky::prelude::*;
use once_cell::unsync::Lazy;

macro_rules! err {
  ($k:ident $data:tt) => {
    Exception { data: ExceptionData::$k $data }
  }
}
macro_rules! throw {
  ($k:ident $data:tt) => {
    Err(err!($k $data))?
  }
}

static mut COLORGEN: Lazy<ColorGenerator> = Lazy::new(|| ColorGenerator::new());

macro_rules! default {
  () => { std::default::Default::default() }
}

#[derive(Debug, Clone)]
enum Expr {
  Val(Val),
  Var(String),
  Ivk(Box<Expr>, Vec<Expr>),
  Set(Box<Expr>, Box<Expr>),
  Dec(Vec<String>),
  Try(Box<Expr>, String),
  Err(Box<Expr>),
  Swi(Box<Expr>, Box<Expr>, Box<Expr>)
}

#[derive(Debug, Clone)]
enum Val {
  Str(String),
  Fun {
    params: Vec<String>,
    locals: Vec<String>,
    body: Vec<Expr>
  },
  Typ(ValType),
  Exc(Exception)
}

impl TryInto<bool> for Val {
  type Error = Exception;

  fn try_into(self) -> Result<bool, Self::Error> {
    match self {
      Self::Str(s) => Ok(!s.is_empty()),
      val => throw!(TypeError { expected: ValType::Str, found: val.into() })
    }
  }
}

impl Default for Val {
    fn default() -> Self {
      Self::Str("".to_string())
    }
}

impl Default for Expr {
    fn default() -> Self {
      Self::Val(default!())
    }
}

impl From<Val> for Expr {
  fn from(value: Val) -> Self {
    Self::Val(value)
  }
}

fn boxed<T>(value: T) -> Box<T> {
  Box::new(value)
}

fn parser() -> impl Parser<char, Vec<Expr>, Error = Simple<char>> {
  let expr = recursive(|expr| {
    let atom = text::ident().map(|s| Expr::Var(s));
    let atom = atom.or(
      just('"')
      .ignore_then(
        take_until(just('"'))
        .map(|(text, _)| Val::Str(text.iter().collect()).into())
      )
    );
    let atom = atom.or(
      text::digits(10)
      .map(|digits| Val::Str(digits).into())
    );
    let atom = atom.or(expr.clone().delimited_by(just('('), just(')')));
    let set = just('$')
      .ignore_then(expr.clone())
      .then_ignore(just(':'))
      .then(expr.clone())
      .map(|(name, value)| Expr::Set(boxed(name), boxed(value)));
    let ivk = just("*")
      .ignore_then(expr.clone())
      .then(expr.clone().separated_by(just(',')))
      .map(|(body, args)| Expr::Ivk(boxed(body), args));
    let fun = just('&')
      .ignore_then(
        text::ident()
        .separated_by(just(","))
        .delimited_by(just('['), just(']'))
        .or(empty().to(vec![]))
      )
      .then(
        text::ident()
        .separated_by(just(","))
        .delimited_by(just('|'), just('|'))
        .or(empty().to(vec![]))
      )
      .then(
        expr.clone()
        .separated_by(just(';'))
        .delimited_by(just('('), just(')'))
      ).map(|((locals, params), body)| Val::Fun { locals, params, body }.into());
    let cmt = just::<_, _, Simple<char>>('#')
      .then_ignore(take_until(just('#')))
      .ignore_then(expr.clone());
    let r#try = text::ident()
      .then_ignore(just('?'))
      .then(expr.clone())
      .map(|(value, expr)| Expr::Try(boxed(expr), value));
    let err = just('!').ignore_then(expr.clone()).map(|message| Expr::Err(boxed(message)));
    let swi = just('%')
      .ignore_then(expr.clone())
      .then_ignore(just(':'))
      .then(
        expr.clone()
        .then_ignore(just('|'))
        .then(expr.clone())
      )
      .map(|(cond, (truthy, falsy))| Expr::Swi(boxed(cond), boxed(truthy), boxed(falsy)));
    let expr_atom = choice([ivk.boxed(), fun.boxed(), err.boxed(), swi.boxed(), r#try.boxed(), atom.boxed(), set.boxed(), cmt.boxed()]);
    expr_atom
  });
  expr.separated_by(just(';')).then_ignore(end())
}

use std::{collections::HashMap, ops::{Index, Range}, iter::zip, fmt::Display, default, hash::Hash, cell::{RefCell, Ref, RefMut}};

struct Closure<'a> {
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
  fn find_owner(&'a self, var: &'a str) -> Option<&'a Closure<'a>> {
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

impl Display for Val {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Val::Str(str) => write!(f, "{}", str),
      Val::Fun { params, locals, body } => write!(f, "fun({}) [{}] {body:?}", params.join(", "), locals.join(", ")),
      Val::Typ(typ) => write!(f, "typ {typ:?}"),
      Val::Exc(exc) => write!(f, "Exception: {exc}"),
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValType {
  Str,
  Fun,
  Typ,
  Exc
}

// struct Frame(String, Range<usize>);

#[derive(Clone, Debug)]
enum ExceptionData {
  TypeError { expected: ValType, found: ValType },
  ArgError { expected: usize, found: usize },
  VarError(String),
  ThrownError(Box<Expr>)
}

#[derive(Clone, Debug)]
struct Exception {
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
      ExceptionData::ThrownError(err) => write!(f, "{:?}", err),
    }
  }
}

// impl Exception {
//   fn add_frame(&mut self, frame: Frame) -> Self {
//     self.traceback.push(frame)
//   }
// }


impl From<Val> for ValType {
  fn from(value: Val) -> Self {
    match value {
        Val::Str(_) => Self::Str,
        Val::Fun { .. } => Self::Fun,
        Val::Typ(_) => Self::Typ,
        Val::Exc(_) => Self::Exc,
    }
  }
}

impl From<bool> for Val {
  fn from(value: bool) -> Self {
    Self::Str(if value { "1" } else { "" }.into())
  }
}

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source};

impl Expr {
  fn eval(self, closure: &mut Closure) -> Result<Val, Exception> {
    Ok(match self {
      Expr::Val(val) => val,
      Expr::Var(name) => closure.lookup(&*name).ok_or_else(|| err!(VarError(name.clone())))?.clone(),
      Expr::Ivk(fun, args) => {
        match fun.eval(closure)? {
          Val::Fun { params, locals, body } => {
            if args.len() != params.len() {
              throw!(ArgError { expected: params.len(), found: args.len() })
            }
            let mut vars = HashMap::<String, Val>::new();
            for local in locals {
              vars.insert(local, default!());
            }
            for (arg, param) in zip(args, params) {
              vars.insert(param, arg.eval(closure)?);
            }
            let mut ivk_closure = closure.extend(vars);
            let (ret, body) = body.split_last().expect("empty funs should be disallowed by the parser");
            for expr in body {
              expr.clone().eval(&mut ivk_closure)?;
            }
            ret.clone().eval(&mut ivk_closure)?
          },
          val => throw!(TypeError { expected: ValType::Fun, found: val.into() })
        }
      },
      Expr::Set(_, _) => todo!(),
      Expr::Try(run, ret) => {
        let result = run.eval(closure);
        let was_ok = result.is_ok();
        closure.find_owner(&ret).or(Some(&closure)).unwrap().vars_mut().insert(ret.clone(), result.unwrap_or_else(|err| Val::Exc(err)));
        return Ok(was_ok.into());
      },
      Expr::Dec(_) => todo!(),
      Expr::Err(err) => throw!(ThrownError(err)),
      Expr::Swi(c, t, f) => if c.eval(closure)?.try_into()? { t.eval(closure)? } else { f.eval(closure)? },
    })
  }

  // fn validate(&self) -> Vec<ValidationErr> {
    
  // }
}

fn main() {
  let filename = "test.txt";
  let src = std::fs::read_to_string(filename).expect("Example file should be readable");
  println!("parsing");
  match parser().parse(src.clone()) {
    Ok(exprs) => {
      let mut closure = Closure::new();
      for expr in exprs {
        println!("{:?}", expr.eval(&mut closure));
      }
    },
    Err(errors) => {
      let mut report = Report::build(ReportKind::Error, filename, 0)
        .with_message("Invalid syntax");
      for err in errors {
        // Safety: there is no multithreading in this program
        let color = unsafe { COLORGEN.next() };
        report = report
          .with_label(
            Label::new((filename, err.span()))
              .with_color(color)
              .with_message(format!(
                "Unexpected {}",
                match err.found() {
                  None => "end of input".to_string(),
                  Some(c) => format!("'{c}'").fg(color).to_string()
                }
              ))
          );
        for expect in err.expected() {
          /* todo */
        }
      }
      report.finish().print((filename, Source::from(src))).expect("Failed to print parse failure")
    }
  }
}
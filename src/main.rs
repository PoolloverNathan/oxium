use chumsky::{prelude::*, Span};
use once_cell::unsync::Lazy;

macro_rules! err {
  ($span:expr, $k:ident $($data:tt)?) => {
    Exception { traceback: vec![$span], data: ExceptionData::$k $($data)? }
  }
}
macro_rules! throw {
  ($span:expr, $k:ident $($data:tt)?) => {
    Err(err!($span, $k $($data)?))?
  }
}

macro_rules! coerce {
  ($span:expr, $val:expr => $ty:ident) => {
    match $val.type_of() {
      ValType::$ty => $val,
      _ => throw!($span, TypeError { expected: ValType::$ty, found: $val.type_of() })
    }
  }
}

static mut COLORGEN: Lazy<ColorGenerator> = Lazy::new(|| ColorGenerator::new());

macro_rules! default {
  () => { std::default::Default::default() }
}

type Origin = (String, Range<usize>);

#[derive(Debug, Clone)]
enum Expr {
  Val(Origin, Val),
  Ary(Origin, Vec<Expr>),
  Var(Origin, String),
  Ivk(Origin, Box<Expr>, Vec<Expr>),
  Set(Origin, Box<Expr>, Box<Expr>),
  Dec(Origin, Vec<String>),
  Try(Origin, Box<Expr>, Box<Expr>),
  Err(Origin, Box<Expr>),
  Swi(Origin, Box<Expr>, Box<Expr>, Box<Expr>),
  Eac(Origin, Box<Expr>, Box<Expr>),
  Evl(Origin, Box<Expr>)
}

#[derive(Debug, Clone)]
enum Val {
  Str(String),
  Ary(Vec<Val>),
  Fun {
    params: Vec<String>,
    locals: Vec<String>,
    body: Vec<Expr>
  },
  Typ(ValType),
  Exc(Exception),
  Nil
}

impl Into<bool> for &Val {
  fn into(self) -> bool {
    match self {
      Val::Str(s) => !s.is_empty(),
      Val::Ary(a) => !a.is_empty() && !a.iter().any(Into::into),
      Val::Fun { params, locals, body } => true,
      Val::Typ(t) => *t != ValType::Nil,
      Val::Exc(_) => false,
      Val::Nil => false,
    }
  }
}

impl Default for Val {
    fn default() -> Self {
      Self::Nil
    }
}

impl From<(Origin, Val)> for Expr {
  fn from(value: (Origin, Val)) -> Self {
    Self::Val(value.0, value.1)
  }
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

fn parser<'a>(filename: &'a str) -> impl Parser<char, Vec<Expr>, Error = Simple<char>> + 'a {
  let expr = recursive(|expr| {
    let atom = text::ident().map_with_span(|s: String, span| Val::from(s).with_span((filename.to_owned(), span)));
    let atom = atom.or(
      just('"')
      .ignore_then(
        take_until(just('"'))
        .map_with_span(|(text, _), span| Val::Str(text.iter().collect()).with_span((filename.to_owned(), span)))
      )
    );
    let atom = atom.or(
      text::digits(10)
      .map_with_span(|digits, span| Val::from(digits).with_span((filename.to_owned(), span)))
    );
    let atom = atom.or(expr.clone().delimited_by(just('('), just(')')));
    let atom = atom.or(
      expr.clone()
        .separated_by(just(","))
        .delimited_by(just("["), just("]"))
        .map_with_span(|items, span| Expr::Ary((filename.to_owned(), span), items))
    );
    let set = expr.clone()
      .delimited_by(just('$'), just('='))
      .then(expr.clone())
      .map_with_span(|(name, value), span| Expr::Set((filename.to_owned(), span), boxed(name), boxed(value)));
    let ivk = just("*")
      .ignore_then(expr.clone())
      .then(expr.clone().separated_by(just(',')).delimited_by(just('('), just(')')))
      .map_with_span(|(body, args), span| Expr::Ivk((filename.to_owned(), span), boxed(body), args));
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
      ).map_with_span(|((locals, params), body), span| Val::Fun { locals, params, body }.with_span((filename.to_owned(), span)));
    let cmt = just::<_, _, Simple<char>>('#')
      .then_ignore(take_until(just('#')))
      .ignore_then(expr.clone());
    let r#try = expr.clone()
      .delimited_by(just('?'), just(':'))
      .then(expr.clone())
      .map_with_span(|(ret, expr), span| Expr::Try((filename.to_owned(), span), boxed(expr), boxed(ret)));
    let err = just('!').ignore_then(expr.clone()).map_with_span(|message, span| Expr::Err((filename.to_owned(), span), boxed(message)));
    let swi = just('%')
      .ignore_then(expr.clone())
      .then_ignore(just(':'))
      .then(
        expr.clone()
        .then_ignore(just('|'))
        .then(expr.clone())
      )
      .map_with_span(|(cond, (truthy, falsy)), span| Expr::Swi((filename.to_owned(), span), boxed(cond), boxed(truthy), boxed(falsy)));
    let eac = expr.clone()
      .delimited_by(just('@'), just('*'))
      .then(expr.clone())
      .map_with_span(|(list, func), span| Expr::Eac((filename.to_owned(), span), boxed(func), boxed(list))); 
    let expr_atom = choice([ivk.boxed(), fun.boxed(), eac.boxed(), err.boxed(), swi.boxed(), r#try.boxed(), atom.boxed(), set.boxed(), cmt.boxed()]);
    expr_atom
  });
  expr.padded().separated_by(just(';')).then_ignore(end())
}

use std::{collections::{HashMap, HashSet}, iter::zip, fmt::Display, cell::{RefCell, Ref, RefMut}, str::FromStr, num::{ParseFloatError}, ops::Range, process::ExitCode};

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

impl Display for Val {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Val::Str(str) => write!(f, "{}", str),
      Val::Ary(ary) => write!(f, "[{}]", ary.iter().map(|a| format!("{}", a)).collect::<Vec<String>>().join(", ")),
      Val::Fun { params, locals, body } => write!(f, "fun({}) [{}] {body:?}", params.join(", "), locals.join(", ")),
      Val::Typ(typ) => write!(f, "typ {typ:?}"),
      Val::Exc(exc) => write!(f, "Exception: {exc}"),
      Val::Nil => write!(f, "∅")
    }
  }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum ValType {
  Str,
  Ary,
  Fun,
  Typ,
  Exc,
  Nil
}

// struct Frame(String, Range<usize>);

#[derive(Clone, Debug)]
enum ExceptionData {
  TypeError { expected: ValType, found: ValType },
  ArgError { expected: usize, found: usize },
  VarError(String),
  NumError(ParseFloatError),
  ThrownError(Box<Expr>),
  AssignError,
  EvlError
}
impl ExceptionData {
    fn code(&self) -> u16 {
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
struct Exception {
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

impl Exception {
  fn add_frame(mut self, frame: Origin) -> Self {
    self.traceback.push(frame);
    self
  }
}

macro_rules! extract {
  ($output:expr, $input:expr, $pat:pat) => {
    match $input {
      $pat => Some($output),
      _ => None
    }
  };
}

impl Val {
  fn type_of(&self) -> ValType {
    match self {
      Self::Str(_) => ValType::Str,
      Self::Ary(_) => ValType::Ary,
      Self::Fun { .. } => ValType::Fun,
      Self::Typ(_) => ValType::Typ,
      Self::Exc(_) => ValType::Exc,
      Self::Nil => ValType::Nil
    }
  }
  fn unbox_str(&self, span: Origin) -> Result<&String, Exception> {
    if let Val::Str(str) = coerce!(span, self => Str) {
      Ok(&str)
    } else {
      unreachable!()
    }
  }
  fn unbox_ary(&self, span: Origin) -> Result<&Vec<Val>, Exception> {
    if let Val::Ary(ary) = coerce!(span, self => Ary) {
      Ok(&ary)
    } else {
      unreachable!()
    }
  }
  fn unbox_fun(&self, span: Origin) -> Result<(&Vec<String>, &Vec<String>, &Vec<Expr>), Exception> {
    if let Val::Fun { params, locals, body } = coerce!(span, self => Fun) {
      Ok((&params, &locals, &body))
    } else {
      unreachable!()
    }
  }
  fn unbox_typ(&self, span: Origin) -> Result<&ValType, Exception> {
    if let Val::Typ(typ) = coerce!(span, self => Typ) {
      Ok(&typ)
    } else {
      unreachable!()
    }
  }
  fn unbox_exc(&self, span: Origin) -> Result<&Exception, Exception> {
    if let Val::Exc(exc) = coerce!(span, self => Exc) {
      Ok(&exc)
    } else {
      unreachable!()
    }
  }
}

impl From<bool> for Val {
  fn from(value: bool) -> Self {
    value.then(|| String::new()).into()
  }
}

impl<T> From<Option<T>> for Val where T: Into<Val> {
  fn from(value: Option<T>) -> Self {
      return value.map_or(Val::Nil, Into::into)
  }
}

use ariadne::{Color, ColorGenerator, Fmt, Label, Report, ReportKind, Source, ReportBuilder};

impl Expr {
  fn eval(self, closure: &mut Closure) -> Result<Val, Exception> {
    Ok(match self {
      Expr::Val(span, val) => val,
      Expr::Ary(span, items) => Val::Ary({
        let mut out = Vec::with_capacity(items.len());
        for item in items {
          out.push(item.eval(closure)?);
        }
        out
      }),
      Expr::Var(span, name) => closure.lookup(&*name).ok_or_else(|| err!(span, VarError(name.clone())))?.clone(),
      Expr::Ivk(span, fun, args) => {
        let fun = fun.eval(closure)?;
        let args = Val::eval_args(args, closure)?;
        fun.invoke(span, args, &closure)?
      },
      Expr::Set(span, name, value) => {
        name.assign(value.eval(closure)?, closure)?
      },
      Expr::Try(span, run, ret) => {
        let result = run.eval(closure);
        let was_ok = result.is_ok();
        ret.assign(result.unwrap_or_else(|e| Val::Exc(e).into()), closure)?;
        return Ok(was_ok.into());
      },
      Expr::Dec(span, _) => todo!(),
      Expr::Err(span, err) => throw!(span, ThrownError(err)),
      Expr::Swi(span, c, t, f) => if (&c.eval(closure)?).into() { t.eval(closure)? } else { f.eval(closure)? },
      Expr::Eac(span, func, list) => {
        let list = list.eval(closure)?.unbox_ary(span.clone())?.clone();
        let func = func.eval(closure)?;
        let func = coerce!(span.clone(), func => Fun);
        let mut out = Vec::<Val>::with_capacity(list.len());
        for item in list {
          out.push(func.clone().invoke(span.clone(), vec![item], closure)?);
        }
        Val::Ary(out)
      },
      Expr::Evl(span, code) => todo!()
    })
  }

  fn assign(self, value: Val, closure: &mut Closure) -> Result<Val, Exception> {
    match self {
        Expr::Val(span, _) => throw!(span, AssignError),
        Expr::Ary(span, t) => {
          let items = extract!(items, coerce!(span.clone(), value => Ary), Val::Ary(items)).unwrap();
          if items.len() == t.len() {
            Val::assign_args(t, items, closure)
          } else {
            throw!(span, ArgError { expected: items.len(), found: t.len() })
          }
        },
        Expr::Var(span, name) => {
          let owner = closure.find_owner(&name).unwrap_or(closure);
          Ok(owner.vars_mut().insert(name, value).unwrap_or(Val::Nil))
        },
        Expr::Ivk(span, _, _) => throw!(span, AssignError),
        Expr::Set(span, target, extra) => {
          target.assign(extra.assign(value, closure)?, closure)
        },
        Expr::Dec(span, _) => todo!(),
        Expr::Try(span, expr, ret) => {
          let result = expr.assign(value, closure);
          let was_ok = result.is_ok();
          ret.assign(result.unwrap_or_else(|e| Val::Exc(e).into()), closure)?;
          Ok(was_ok.into())
        },
        Expr::Err(span, _) => throw!(span, AssignError),
        Expr::Swi(span, cond, t, f) => {
          (if (&cond.eval(closure)?).into() { t } else { f }).assign(value, closure)
        },
        Expr::Eac(span, _, _) => throw!(span, AssignError),
        Expr::Evl(span, code) => todo!()
    }
  }

  // fn validate(&self) -> Vec<ValidationErr> {
    
  // }
}

macro_rules! option_match {
  ($pat:pat = $expr:expr => $val:expr) => {
    match $expr {
      $pat => Some($val),
      _ => None
    }
  };
}

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

impl From<String> for Val {
  fn from(value: String) -> Self {
    Val::Str(value)
  }
}
impl From<Vec<Val>> for Val {
  fn from(value: Vec<Val>) -> Self {
    Val::Ary(value)
  }
}
impl From<ValType> for Val {
  fn from(value: ValType) -> Self {
    Val::Typ(value)
  }
}
impl From<Exception> for Val {
  fn from(value: Exception) -> Self {
    Val::Exc(value)
  }
}

fn with_span<F: WithSpan<S, T>, T: From<(S, F)>, S: Span>(span: S) -> impl Fn(F) -> T {
  move |value| value.with_span(span.clone())
}

impl Val {
  fn invoke(self, span: Origin, args: Vec<Val>, closure: &Closure) -> Result<Val, Exception> {
    if let Val::Str(ref data) = self {
      if data.len() == 1 {
        match &**data {
          "-" => {
            if args.len() != 2 {
              throw!(span.clone(), ArgError { expected: 2, found: args.len() })
            }
            return if let (Val::Str(a), Val::Str(b)) = if let [a, b] = &args[..] {
              coerce!(span.clone(), a => Str);
              coerce!(span.clone(), b => Str);
              (a, b)
            } else {
              unreachable!()
            } {
              let a = f64::from_str(a).map_err(|e| err!(span.clone(), NumError(e)))?;
              let b = f64::from_str(b).map_err(|e| err!(span.clone(), NumError(e)))?;
              Ok((b - a).to_string().into())
            } else {
              unreachable!()
            }
          },
          _ => {}
        }
      }
    }
    let (params, locals, body) = self.unbox_fun(span.clone())?;
    if args.len() != params.len() {
      throw!(span.clone(), ArgError { expected: params.len(), found: args.len() })
    }
    let mut vars = HashMap::<String, Val>::new();
    for local in locals {
      vars.insert(local.to_owned(), default!());
    }
    for (arg, param) in zip(args, params) {
      vars.insert(param.to_owned(), arg);
    }
    let mut ivk_closure = closure.extend(vars);
    let (ret, body) = body.split_last().expect("empty funs should be disallowed by the parser");
    for expr in body {
      expr.clone().eval(&mut ivk_closure).map_err(|e| e.add_frame(span.clone()))?;
    }
    ret.clone().eval(&mut ivk_closure).map_err(|e| e.add_frame(span))
  }

  fn eval_args(args: Vec<Expr>, closure: &mut Closure) -> Result<Vec<Val>, Exception> {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
      out.push(arg.eval(closure)?)
    }
    Ok(out)
  }
  fn assign_args(args: Vec<Expr>, vals: Vec<Val>, closure: &mut Closure) -> Result<Val, Exception> {
    let mut out = Vec::with_capacity(args.len());
    for (arg, val) in zip(args, vals) {
      out.push(arg.assign(val, closure)?);
    }
    Ok(out.into())
  }
}

fn main() -> ExitCode {
  let filename = "test.txt";
  let src = std::fs::read_to_string(filename).expect("Example file should be readable");
  println!("parsing");
  match parser(filename).parse(src.clone()) {
    Ok(exprs) => {
      let mut closure = Closure::new();
      for expr in exprs {
        match expr.eval(&mut closure) {
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
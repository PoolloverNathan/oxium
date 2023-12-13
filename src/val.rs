use super::*;
use std::convert::Infallible as No;

#[derive(Debug, Clone)]
pub enum Val {
  Str(String),
  Ary(Vec<Val>),
  Fun {
    params: Vec<String>,
    locals: Vec<String>,
    body: Vec<Expr>
  },
  Typ(ValType),
  Exc(Exception),
  Thr(No),
  Nil
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ValType {
  Str,
  Ary,
  Fun,
  Typ,
  Exc,
  Thr,
  Nil
}

impl Val {
  pub fn type_of(&self) -> ValType {
    match self {
      Self::Str(_) => ValType::Str,
      Self::Ary(_) => ValType::Ary,
      Self::Fun { .. } => ValType::Fun,
      Self::Typ(_) => ValType::Typ,
      Self::Exc(_) => ValType::Exc,
      Self::Thr(_) => ValType::Thr,
      Self::Nil => ValType::Nil
    }
  }
  pub fn unbox_str(&self, span: Origin) -> Result<&String, Exception> {
    if let Val::Str(str) = coerce!(span, self => Str) {
      Ok(&str)
    } else {
      unreachable!()
    }
  }
  pub fn unbox_ary(&self, span: Origin) -> Result<&Vec<Val>, Exception> {
    if let Val::Ary(ary) = coerce!(span, self => Ary) {
      Ok(&ary)
    } else {
      unreachable!()
    }
  }
  pub fn unbox_fun(&self, span: Origin) -> Result<(&Vec<String>, &Vec<String>, &Vec<Expr>), Exception> {
    if let Val::Fun { params, locals, body } = coerce!(span, self => Fun) {
      Ok((&params, &locals, &body))
    } else {
      unreachable!()
    }
  }
  pub fn unbox_typ(&self, span: Origin) -> Result<&ValType, Exception> {
    if let Val::Typ(typ) = coerce!(span, self => Typ) {
      Ok(&typ)
    } else {
      unreachable!()
    }
  }
  pub fn unbox_exc(&self, span: Origin) -> Result<&Exception, Exception> {
    if let Val::Exc(exc) = coerce!(span, self => Exc) {
      Ok(&exc)
    } else {
      unreachable!()
    }
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

impl Val {
  pub fn invoke(self, span: Origin, args: Vec<Val>, closure: &Closure) -> Result<Val, Exception> {
    // if let Val::Str(ref data) = self {
    //   if data.len() == 1 {
    //     match &**data {
    //       "-" => {
    //         if args.len() != 2 {
    //           throw!(span.clone(), ArgError { expected: 2, found: args.len() })
    //         }
    //         return if let (Val::Str(a), Val::Str(b)) = if let [a, b] = &args[..] {
    //           coerce!(span.clone(), a => Str);
    //           coerce!(span.clone(), b => Str);
    //           (a, b)
    //         } else {
    //           unreachable!()
    //         } {
    //           let a = f64::from_str(a).map_err(|e| err!(span.clone(), NumError(e)))?;
    //           let b = f64::from_str(b).map_err(|e| err!(span.clone(), NumError(e)))?;
    //           Ok((b - a).to_string().into())
    //         } else {
    //           unreachable!()
    //         }
    //       },
    //       _ => {}
    //     }
    //   }
    // }
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
      expr.clone().eval(&mut ivk_closure).with_frame(span.clone())?;
    }
    ret.clone().eval(&mut ivk_closure).with_frame(span)
  }

  pub fn eval_args(args: Vec<Expr>, closure: &Closure) -> Result<Vec<Val>, Exception> {
    let mut out = Vec::with_capacity(args.len());
    for arg in args {
      out.push(arg.eval(closure)?)
    }
    Ok(out)
  }
  pub fn assign_args(args: Vec<Expr>, vals: Vec<Val>, closure: &Closure) -> Result<Val, Exception> {
    let mut out = Vec::with_capacity(args.len());
    for (arg, val) in zip(args, vals) {
      out.push(arg.assign(val, closure)?);
    }
    Ok(out.into())
  }
}

impl Into<bool> for &Val {
  fn into(self) -> bool {
    match self {
      Val::Str(s) => !s.is_empty(),
      Val::Ary(a) => !a.is_empty() && !a.iter().any(Into::into),
      Val::Fun { .. } => true,
      Val::Typ(t) => *t != ValType::Nil,
      Val::Exc(_) => false,
      Val::Thr(_) => todo!(),
      Val::Nil => false,
    }
  }
}

impl Default for Val {
  fn default() -> Self {
    Self::Nil
  }
}

impl Display for Val {
  fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
    match self {
      Val::Str(str) => write!(f, "{}", str),
      Val::Ary(ary) => write!(f, "[{}]", ary.iter().map(|a| format!("{}", a)).collect::<Vec<String>>().join(", ")),
      Val::Fun { params, locals, body } => write!(f, "fun({}) [{}] {body:?}", params.join(", "), locals.join(", ")),
      Val::Typ(typ) => write!(f, "<{typ:?}>"),
      Val::Exc(exc) => write!(f, "[E{:08}: {exc}]", exc.data.code()),
      Val::Thr(_) => todo!(),
      Val::Nil => write!(f, "∅")
    }
  }
}

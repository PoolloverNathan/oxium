use super::*;

pub type Origin = (String, Range<usize>);

#[derive(Debug, Clone)]
pub enum Expr {
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
  Yld(Origin, Box<Expr>),
  Evl(Origin, Box<Expr>)
}

impl From<(Origin, Val)> for Expr {
  fn from(value: (Origin, Val)) -> Self {
    Self::Val(value.0, value.1)
  }
}

impl Expr {
  pub fn eval(self, closure: &Closure) -> Result<Val, Exception> {
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
      Expr::Yld(span, val) => todo!(),
      Expr::Evl(span, code) => todo!(),
    })
  }

  pub fn assign(self, value: Val, closure: &Closure) -> Result<Val, Exception> {
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
        Expr::Yld(span, val) => todo!(),
        Expr::Evl(span, code) => todo!(),
    }
  }

  pub fn origin(&self) -> &Origin {
    match self {
      | Expr::Val(o, _)
      | Expr::Ary(o, _)
      | Expr::Var(o, _)
      | Expr::Ivk(o, _, _)
      | Expr::Set(o, _, _)
      | Expr::Dec(o, _)
      | Expr::Try(o, _, _)
      | Expr::Err(o, _)
      | Expr::Swi(o, _, _, _)
      | Expr::Eac(o, _, _)
      | Expr::Yld(o, _)
      | Expr::Evl(o, _)
      => o
    }
  }

  // fn validate(&self) -> Vec<ValidationErr> {
    
  // }
}

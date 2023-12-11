use super::*;

pub(crate) fn parser<'a>(filename: &'a str) -> impl Parser<char, Vec<Expr>, Error = Simple<char>> + 'a {
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
  expr.padded_by(just('.').repeated()).separated_by(just(';')).then_ignore(end())
}

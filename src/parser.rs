use std::cell::RefCell;
use std::ops::Range;

use nom::branch::alt;
use nom::bytes::complete::{take, take_till1, take_while};
use nom::character::complete::{anychar, char, multispace0};
use nom::combinator::{all_consuming, map, not, recognize, rest, verify};
use nom::sequence::{delimited, preceded, terminated};

use crate::types::*;

use std::fmt::{self, Binary};

use super::*;
use nom::{
    bytes::complete::tag,
    character::complete::{alpha1, alphanumeric1, digit1, multispace1},
    combinator::opt,
    multi::{many0, many1, separated_list0},
    sequence::{pair, tuple},
};
type LocatedSpan<'a> = nom_locate::LocatedSpan<&'a str, State<'a>>;
type IResult<'a, T> = nom::IResult<LocatedSpan<'a>, T>;

#[derive(Debug, PartialEq)]
pub struct PosInfo(u32, Range<usize>);
trait Position {
    fn to_position(&self) -> PosInfo;
}
#[derive(Debug, PartialEq)]
pub struct Error {
    pub pi: PosInfo,
    pub msg: String,
}

impl<'a> Position for LocatedSpan<'a> {
    fn to_position(&self) -> PosInfo {
        let start = self.location_offset();
        let end = start + self.fragment().len();
        PosInfo(self.location_line(), start..end)
    }
}

#[derive(Clone, Debug)]
pub struct State<'a>(&'a RefCell<Vec<Error>>);

impl<'a> State<'a> {
    pub fn report_error(&self, error: Error) {
        self.0.borrow_mut().push(error);
    }
}

fn ws<'a, F, T>(parser: F) -> impl FnMut(LocatedSpan<'a>) -> IResult<T>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
{
    delimited(multispace0, parser, multispace0)
}

fn expect<'a, F, E, T>(
    mut parser: F,
    error_msg: E,
) -> impl FnMut(LocatedSpan<'a>) -> IResult<Option<T>>
where
    F: FnMut(LocatedSpan<'a>) -> IResult<T>,
    E: ToString,
{
    move |input| match parser(input) {
        Ok((remaining, out)) => Ok((remaining, Some(out))),
        Err(nom::Err::Error(nom::error::Error { input: i, .. }))
        | Err(nom::Err::Failure(nom::error::Error { input: i, .. })) => {
            let err = Error {
                pi: i.to_position(),
                msg: error_msg.to_string(),
            };
            i.extra.report_error(err);
            Ok((i, None))
        }
        Err(err) => Err(err),
    }
}

fn funccall(input: LocatedSpan) -> IResult<FuncCall> {
    let call = tuple((
        ws(ident),
        ws(char('(')),
        separated_list0(ws(char(',')), exp),
        ws(char(')')),
    ));
    map(call, |(id, _, args, _)| FuncCall {
        name: id.name,
        args,
    })(input)
}

fn number(input: LocatedSpan) -> IResult<Exp> {
    map(digit1, |span: LocatedSpan| {
        Exp::Number(match span.fragment().parse::<i32>() {
            Ok(n) => Number::Integer(n),
            Err(_) => Number::Error,
        })
    })(input)
}

fn ident(input: LocatedSpan) -> IResult<Ident> {
    let p = recognize(pair(
        alt((alpha1, tag("_"))),
        many0(alt((alphanumeric1, tag("_")))),
    ));
    map(ws(p), |span: LocatedSpan| Ident {
        name: span.fragment().to_string(),
        value: Value::Error,
    })(input)
}

fn opchar(input: LocatedSpan) -> IResult<char> {
    alt((
        char('+'),
        char('-'),
        char('*'),
        char('/'),
        char('<'),
        char('>'),
    ))(input)
}

fn op(input: LocatedSpan) -> IResult<Op> {
    map(ws(many1(opchar)), |chars| {
        let s: String = chars.into_iter().collect();
        match s.as_str() {
            "+" => Op::Plus,
            "-" => Op::Subtract,
            "*" => Op::Multiply,
            "/" => Op::Divide,
            "<" => Op::LessThan,
            _ => Op::Error,
        }
    })(input)
}

pub fn binop(input: LocatedSpan) -> IResult<(Vec<Exp>, Vec<Op>)> {
    let parser = tuple((ws(simple_exp), op, ws(exp_rec)));
    map(ws(parser), |(lhs, op, mut rhs)| {
        rhs.0.push(lhs);
        rhs.1.push(op);
        rhs
    })(input)
}

fn simple_exp(input: LocatedSpan) -> IResult<Exp> {
    alt((map(funccall, Exp::FuncCall), number, map(ident, Exp::Ident)))(input)
}

pub fn exp_rec(input: LocatedSpan) -> IResult<(Vec<Exp>, Vec<Op>)> {
    alt((binop, map(simple_exp, |e| (vec![e], vec![]))))(input)
}

pub fn exp(input: LocatedSpan) -> IResult<Exp> {
    map(exp_rec, |(mut exps, mut ops)| {
        if ops.is_empty() {
            exps.pop().unwrap()
        } else {
            Exp::Binop({
                // We don't have to reverse these vectors.
                // we do that simply to print the structure as it is
                exps.reverse();
                ops.reverse();
                Binop { exps, ops }
            })
        }
    })(input)
}

fn ret(input: LocatedSpan) -> IResult<LastStat> {
    map(tuple((tag("return"), multispace1, exp)), |(_, _, e)| {
        LastStat::Return(Return { exps: vec![e] })
    })(input)
}

fn brk(input: LocatedSpan) -> IResult<LastStat> {
    map(tag("break"), |span: LocatedSpan| {
        LastStat::Break(span.fragment().to_string())
    })(input)
}

fn laststat(input: LocatedSpan) -> IResult<LastStat> {
    ws(alt((ret, brk)))(input)
}

fn namelist(input: LocatedSpan) -> IResult<Vec<String>> {
    map(separated_list0(char(','), ws(ident)), |v| {
        v.into_iter().map(|x| x.name).collect()
    })(input)
}

fn block(input: LocatedSpan) -> IResult<Block> {
    alt((
        map(chunk, Block::Chunk),
        map(take(0usize), |_| Block::Error),
    ))(input)
}

fn funcbody(input: LocatedSpan) -> IResult<FuncBody> {
    let paramlist = delimited(ws(char('(')), namelist, ws(char(')')));
    map(tuple((paramlist, block)), |(v, b)| -> FuncBody {
        FuncBody {
            params: v,
            block: b,
        }
    })(input)
}

fn funcdef(input: LocatedSpan) -> IResult<Stat> {
    let is_function = |i| {
        map(ws(tag("function")), |span: LocatedSpan| {
            span.fragment().eq(&"function")
        })(i)
    };

    let is_end = |i| {
        map(ws(tag("end")), |span: LocatedSpan| {
            span.fragment().eq(&"end")
        })(i)
    };

    map(
        tuple((is_function, ident, funcbody, is_end)),
        |(def_begin, name, body, def_end)| {
            if (def_begin && def_end) {
                Stat::FuncDef(FuncDef {
                    body,
                    name: name.name,
                })
            } else {
                Stat::Error
            }
        },
    )(input)
}

fn cond(input: LocatedSpan) -> IResult<Stat> {
    let first_cond = map(
        tuple((ws(tag("if")), exp, ws(tag("then")), block)),
        |(_, e, _, b)| (e, b),
    );
    let mid_conds = map(
        tuple((ws(tag("elseif")), exp, ws(tag("then")), block)),
        |(_, e, _, b)| (e, b),
    );
    let last_cond = map(tuple((ws(tag("else")), block)), |(_, b)| (Exp::True, b));

    map(
        tuple((first_cond, many0(mid_conds), opt(last_cond), ws(tag("end")))),
        |(f, m, last, _)| {
            let mut result = vec![f];
            for t in m {
                result.push(t);
            }
            if let Some(l) = last {
                result.push(l)
            };
            Stat::Cond(Cond { pairs: result })
        },
    )(input)
}

pub fn local(input: LocatedSpan) -> IResult<Stat> {
    map(
        tuple((ws(tag("local")), ident, ws(tag("=")), many1(exp))),
        |(_, name, _, explist)| {
            Stat::Local(Local {
                lhs: name,
                rhs: explist,
            })
        },
    )(input)
}

pub fn assign(input: LocatedSpan) -> IResult<Stat> {
    map(
        tuple((ws(ident), ws(tag("=")), many1(exp))),
        |(name, _, explist)| {
            Stat::Assign(Assign {
                lhs: name,
                rhs: explist,
            })
        },
    )(input)
}

fn stat(input: LocatedSpan) -> IResult<Stat> {
    terminated(
        alt((funcdef, map(funccall, Stat::FuncCall), cond, local, assign)),
        ws(opt(char(';'))),
    )(input)
}

fn chunk(input: LocatedSpan) -> IResult<Chunk> {
    let chunk_parser = tuple((many0(stat), opt(laststat)));
    map(
        terminated(chunk_parser, ws(opt(char(';')))),
        |(stats, last_stat)| Chunk {
            stats,
            laststat: last_stat,
        },
    )(input)
}

fn source_file(input: LocatedSpan) -> IResult<Lua> {
    terminated(
        alt((map(chunk, Lua::Chunk), map(take(0usize), |_| Lua::Error))),
        preceded(expect(not(anychar), "expected EOF"), rest),
    )(input)
}

pub fn parse(source: &str) -> (Lua, Vec<Error>) {
    let errors = RefCell::new(Vec::new());
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, lua) = all_consuming(source_file)(input).expect("parser cannot fail");
    (lua, errors.into_inner())
}

pub fn test_parse<T, P>(source: &str, parser: P) -> (T, Vec<Error>)
where
    P: FnMut(LocatedSpan) -> IResult<T>,
{
    let errors = RefCell::new(Vec::new());
    let input = LocatedSpan::new_extra(source, State(&errors));
    let (_, output) = all_consuming(parser)(input).expect("parser cannot fail");
    (output, errors.into_inner())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_laststat() {
        let empty = vec![];
        assert_eq!(&parse("break;").1[..], &empty);
        assert_eq!(&parse("  break      ;  ").1[..], &empty);
        assert_eq!(&parse("  break      \n;  ").1[..], &empty);
        assert_eq!(&parse("  return 1      \n;  ").1[..], &empty);
        assert_eq!(&parse("  return 1   +2   \n;  ").1[..], &empty);
        assert_eq!(&parse("  return n+1;  ").1[..], &empty);
        assert_eq!(&parse("  return 1   +n;  ").1[..], &empty);
        assert_eq!(&parse("  return n;  ").1[..], &empty);
        assert_eq!(&parse("  return a+1+2+d+e;  ").1[..], &empty);
        assert_eq!(&parse("  return f(a+1+2+d+e)+2;  ").1[..], &empty);
    }

    #[test]
    fn test_whatever() {
        let empty = vec![];
        assert_eq!(&test_parse("print(30)", funccall).1[..], &empty);
        assert_eq!(&test_parse("print(fib(30))", funccall).1[..], &empty);
        assert_eq!(
            &test_parse("print(fib(fib(fib(30))))", funccall).1[..],
            &empty
        );
        assert_eq!(
            &test_parse("function fib(n)\n return n; \nend", funcdef).1[..],
            &empty
        );
        assert_eq!(
            &test_parse(
                r#"
            function fib(n)
                  return n;
            end
            "#,
                funcdef
            )
            .1[..],
            &empty
        );

        assert_eq!(
            &test_parse(
                r#"
                    function fib(n)
                   if n < 2 then
                      return n;
                   end
                
                   return n1 + n2;
                end
            "#,
                funcdef
            )
            .1[..],
            &empty
        );

        assert_eq!(
            &test_parse(
                r#"
                    function fib(n)
                   if n < 2 then
                      return n;
                   end
                
                   local n1 = fib(n-1);
                   local n2 = fib(n-2);

                   return n1 + n2;
                end
            "#,
                funcdef
            )
            .1[..],
            &empty
        );

        assert_eq!(
            &test_parse(
                r#"
                    function fib(n)
                   if n < 2 then
                      return n;
                   end
                
                   local n1 = fib(n-1);
                   local n2 = fib(n-2);

                   return n1 + n2;
                end

                print(fib(30));
            "#,
                source_file
            )
            .1[..],
            &empty
        );
    }
}

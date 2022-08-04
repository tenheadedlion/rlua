use std::{
    cell::RefCell,
    collections::HashMap,
    fmt::{self, write},
    hash::Hash,
    mem, ops,
};

use anyhow::{bail, Error, Result};

#[derive(Debug)]
pub struct Chunk {
    pub stats: Vec<Stat>,
    pub laststat: Option<LastStat>,
}

#[derive(Debug)]
pub enum Stat {
    FuncCall(FuncCall),
    FuncDef(FuncDef),
    Cond(Cond),
    Local(Local),
    Assign(Assign),
    Error,
}

#[derive(Debug)]
pub struct Cond {
    pub pairs: Vec<(Exp, Block)>,
}

#[derive(Debug)]
pub struct Local {
    pub lhs: Ident,
    pub rhs: Vec<Exp>,
}

#[derive(Debug)]
pub struct Assign {
    pub lhs: Ident,
    pub rhs: Vec<Exp>,
}

#[derive(Debug)]
pub enum LastStat {
    Return(Return),
    Break(String),
    Error,
}

#[derive(Debug)]
pub struct Return {
    pub exps: Vec<Exp>,
}

#[derive(Debug)]
pub struct FuncDef {
    pub name: String,
    pub body: FuncBody,
}

#[derive(Debug)]
pub struct FuncCall {
    pub name: String,
    pub args: Vec<Exp>,
}

#[derive(Debug)]
pub struct FuncBody {
    pub params: Vec<String>,
    pub block: Block,
}

#[derive(Debug)]
pub enum Block {
    Chunk(Chunk),
    Error,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Number {
    Integer(i32),
    Float(f32),
    Error,
}

#[derive(Debug, Clone, Copy)]
pub enum Op {
    Plus,
    Subtract,
    Multiply,
    Divide,
    LessThan,
    Error,
}

#[derive(Debug)]
pub struct Binop {
    pub exps: Vec<Exp>,
    pub ops: Vec<Op>,
}

impl From<Exp> for String {
    fn from(exp: Exp) -> Self {
        match exp {
            Exp::Ident(i) => i.name,
            Exp::String(i) => i,
            _ => "Error conversion".to_owned(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Number(Number),
    String(String),
    Error,
}

impl fmt::Display for Number {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Number::Error => write!(f, "NaN"),
            Number::Integer(i) => write!(f, "{}", i),
            Number::Float(_f) => write!(f, "{}", _f),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Ident {
    pub name: String,
    pub value: Value,
}

#[derive(Debug)]
pub enum Exp {
    Number(Number),
    String(String),
    Ident(Ident),
    Binop(Binop),
    FuncCall(FuncCall),
    True,
    Error,
}

#[derive(Debug)]
pub enum Lua {
    Chunk(Chunk),
    Error,
}

use std::collections::HashMap;

use crate::ast::*;

#[derive(Debug)]
pub struct IrProg<'a> {
    pub func: IrFunc<'a>,
}

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub name: &'a str,
    pub stmts: Vec<IrStmt>,
    pub locals: u32,
}

#[derive(Debug)]
pub enum IrStmt {
    Const(i32),
    Unary(UnaryOp),
    Binary(BinaryOp),
    Ret,
    FrameAddr(usize),
    Load,
    Store,
    Pop,
}

pub fn ast2ir<'a>(p: &'a Prog<'a>) -> IrProg<'a> {
    let mut ctx: HashMap<&'a str, usize> = HashMap::new();
    IrProg { func: func(&p.func, &mut ctx) }
}

fn func<'a>(f: &Func<'a>, ctx: &mut HashMap<&'a str, usize>) -> IrFunc<'a> {
    let mut stmts = Vec::new();
    let mut locals = 0;
    for x in &f.stmts.stmts {
        if let Stmt::Declaration(_c) = x {
            locals += 1;
        }
    }

    for x in &f.stmts.stmts {
        match x {
            Stmt::Ret(e) => {
                expr(&mut stmts, ctx, &e);
                stmts.push(IrStmt::Ret);
            }
            Stmt::MaybeExpr(e) => {
                if let Some(x) = e {
                    expr(&mut stmts, ctx, &x);
                    stmts.push(IrStmt::Pop);
                }
            }
            Stmt::Declaration(e) => {
                if ctx.contains_key(e.name) {
                    panic!("Variable {} is defined twice", e.name);
                } else {
                    ctx.insert(e.name, ctx.len());
                    if let Some(x) = &e.val {
                        expr(&mut stmts, ctx, &x);
                        stmts.push(IrStmt::FrameAddr(*ctx.get(e.name).unwrap()));
                        stmts.push(IrStmt::Store);
                        stmts.push(IrStmt::Pop);
                    }
                }
            }
        }
    }
    IrFunc { name: f.name, stmts, locals }
}

fn expr<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, e: &Expr) {
    match e {
        Expr::LOr(a) => { logical_or(stmts, ctx, a); }
        Expr::Assign(name, val) => {
            if ctx.contains_key(name) {
                expr(stmts, ctx, &**val);
                stmts.push(IrStmt::FrameAddr(*ctx.get(name).unwrap()));
                stmts.push(IrStmt::Store);
            } else {
                panic!("Variable {} is assigned a value before declaration!", name);
            }
        }
    }
}

fn logical_or<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, e: &LogicalOr) {
    match e {
        LogicalOr::LAnd(a) => { logical_and(stmts, ctx, a); }
        LogicalOr::Bop(a, op, b) => {
            logical_or(stmts, ctx, &**a);
            logical_and(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn logical_and<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, e: &LogicalAnd) {
    match e {
        LogicalAnd::Eqn(a) => { equality(stmts, ctx, a); }
        LogicalAnd::Bop(a, op, b) => {
            logical_and(stmts, ctx, &**a);
            equality(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn equality<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, e: &Equality) {
    match e {
        Equality::Rel(a) => { relational(stmts, ctx, a); }
        Equality::Bop(a, op, b) => {
            equality(stmts, ctx, &**a);
            relational(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn relational<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, e: &Relational) {
    match e {
        Relational::Add(a) => { additive(stmts, ctx, a); }
        Relational::Bop(a, op, b) => {
            relational(stmts, ctx, &**a);
            additive(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn additive<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, a: &Additive) {
    match a {
        Additive::Mul(m) => { multiplicative(stmts, ctx, m); }
        Additive::Bop(a, op, b) => {
            additive(stmts, ctx, &**a);
            multiplicative(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn multiplicative<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, m: &Multiplicative) {
    match m {
        Multiplicative::U(u) => unary(stmts, ctx, u),
        Multiplicative::Mul(m, op, a) => {
            multiplicative(stmts, ctx, &**m);
            unary(stmts, ctx, a);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn unary<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, u: &Unary) {
    match u {
        Unary::Prim(p) => primary(stmts, ctx, p),
        Unary::Uop(op, v) => {
            unary(stmts, ctx, v);
            stmts.push(IrStmt::Unary(*op));
        }
    }
}

fn primary<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut HashMap<&'a str, usize>, p: &Primary) {
    match p {
        Primary::Int(i, _) => {
            stmts.push(IrStmt::Const(*i));
        }
        Primary::Braced(e) => {
            expr(stmts, ctx, e);
        }
        Primary::Identifier(name) => {
            stmts.push(IrStmt::FrameAddr(*ctx.get(name).expect(&format!("{} is referenced before definition!", name))));
            stmts.push(IrStmt::Load);
        }
    }
}

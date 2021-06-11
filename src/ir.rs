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
pub struct Context<'a> {
    pub vars: HashMap<&'a str, usize>,
    pub label: u32,
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
    Label(u32),
    Bnez(u32),
    Beqz(u32),
    Br(u32),
}

impl<'a> Context<'a> {
    pub fn new() -> Context<'a> { Context { vars: HashMap::new(), label: 0 } }
}
pub fn ast2ir<'a>(p: &'a Prog<'a>) -> IrProg<'a> {
    let mut ctx = Context::new();
    IrProg { func: func(&p.func, &mut ctx) }
}

fn func<'a>(f: &Func<'a>, ctx: &mut Context<'a>) -> IrFunc<'a> {
    let mut stmts = Vec::new();
    let mut locals = 0;
    for x in &f.stmts {
        if let BlockItem::Decl(_c) = x {
            locals += 1;
        }
    }

    for x in &f.stmts {
        match x {
            BlockItem::Stmt(st) => {
                statement(&mut stmts, ctx, st);
            }
            BlockItem::Decl(e) => {
                if ctx.vars.contains_key(e.name) {
                    panic!("Variable {} is defined twice", e.name);
                } else {
                    ctx.vars.insert(e.name, ctx.vars.len());
                    if let Some(x) = &e.val {
                        expr(&mut stmts, ctx, &x);
                        stmts.push(IrStmt::FrameAddr(*ctx.vars.get(e.name).unwrap()));
                        stmts.push(IrStmt::Store);
                        stmts.push(IrStmt::Pop);
                    }
                }
            }
        }
    }
    IrFunc { name: f.name, stmts, locals }
}

fn statement<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, st: &Stmt) {
    match st {
        Stmt::Ret(r) => {
            expr(stmts, ctx, &r);
            stmts.push(IrStmt::Ret);
        }
        Stmt::MaybeExpr(e) => {
            if let Some(x) = e {
                expr(stmts, ctx, &x);
                stmts.push(IrStmt::Pop);
            }
        }
        Stmt::If(cond, if_expr, op) => {
            expr(stmts, ctx, cond);
            let no = ctx.label;
            if let Some(x) = op {
                ctx.label = no + 2;
                stmts.push(IrStmt::Beqz(no));
                statement(stmts, ctx, &*if_expr);
                stmts.push(IrStmt::Br(no + 1));
                stmts.push(IrStmt::Label(no));
                statement(stmts, ctx, &*x);
                stmts.push(IrStmt::Label(no + 1));
            } else {
                ctx.label = no + 1;
                stmts.push(IrStmt::Beqz(no));
                statement(stmts, ctx, &*if_expr);
                stmts.push(IrStmt::Label(no));
            }
        }
    }
}

fn conditional<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, o: &Conditional) {
    match o {
        Conditional::LOr(a) => { logical_or(stmts, ctx, &a); }
        Conditional::Cond(cond, t, f) => {
            let no: u32 = ctx.label;
            ctx.label = no + 2;
            logical_or(stmts, ctx, &cond);
            stmts.push(IrStmt::Beqz(no));
            expr(stmts, ctx, &t);
            stmts.push(IrStmt::Br(no + 1));
            stmts.push(IrStmt::Label(no));
            conditional(stmts, ctx, f);
            stmts.push(IrStmt::Label(no + 1));
        }
    }
}

fn expr<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &Expr) {
    match e {
        Expr::Assign(name, val) => {
            if ctx.vars.contains_key(name) {
                expr(stmts, ctx, &**val);
                stmts.push(IrStmt::FrameAddr(*ctx.vars.get(name).unwrap()));
                stmts.push(IrStmt::Store);
            } else {
                panic!("Variable {} is assigned a value before declaration!", name);
            }
        }
        Expr::Cond(a) => {
            conditional(stmts, ctx, a);
        }
    }
}

fn logical_or<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &LogicalOr) {
    match e {
        LogicalOr::LAnd(a) => { logical_and(stmts, ctx, a); }
        LogicalOr::Bop(a, op, b) => {
            logical_or(stmts, ctx, &**a);
            logical_and(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn logical_and<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &LogicalAnd) {
    match e {
        LogicalAnd::Eqn(a) => { equality(stmts, ctx, a); }
        LogicalAnd::Bop(a, op, b) => {
            logical_and(stmts, ctx, &**a);
            equality(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn equality<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &Equality) {
    match e {
        Equality::Rel(a) => { relational(stmts, ctx, a); }
        Equality::Bop(a, op, b) => {
            equality(stmts, ctx, &**a);
            relational(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn relational<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &Relational) {
    match e {
        Relational::Add(a) => { additive(stmts, ctx, a); }
        Relational::Bop(a, op, b) => {
            relational(stmts, ctx, &**a);
            additive(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn additive<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, a: &Additive) {
    match a {
        Additive::Mul(m) => { multiplicative(stmts, ctx, m); }
        Additive::Bop(a, op, b) => {
            additive(stmts, ctx, &**a);
            multiplicative(stmts, ctx, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn multiplicative<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, m: &Multiplicative) {
    match m {
        Multiplicative::U(u) => unary(stmts, ctx, u),
        Multiplicative::Mul(m, op, a) => {
            multiplicative(stmts, ctx, &**m);
            unary(stmts, ctx, a);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn unary<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, u: &Unary) {
    match u {
        Unary::Prim(p) => primary(stmts, ctx, p),
        Unary::Uop(op, v) => {
            unary(stmts, ctx, v);
            stmts.push(IrStmt::Unary(*op));
        }
    }
}

fn primary<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, p: &Primary) {
    match p {
        Primary::Int(i, _) => {
            stmts.push(IrStmt::Const(*i));
        }
        Primary::Braced(e) => {
            expr(stmts, ctx, e);
        }
        Primary::Identifier(name) => {
            stmts.push(IrStmt::FrameAddr(*ctx.vars.get(name).expect(&format!("{} is referenced before definition!", name))));
            stmts.push(IrStmt::Load);
        }
    }
}

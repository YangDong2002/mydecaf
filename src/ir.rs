use std::collections::HashMap;

use crate::ast::*;
use crate::consteval::const_expr;

#[derive(Debug)]
pub struct IrProg<'a> {
    pub funcs: Vec<IrFunc<'a>>,
    pub global_initialized: Vec<(&'a str, i32)>,
    pub global_uninitialized: Vec<&'a str>,
}

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub name: &'a str,
    pub stmts: Vec<IrStmt>,
    pub locals: u32,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum NameStatus {
    FuncDeclared(usize, usize),
    // position, #args
    FuncImplemented(usize, usize),
    // position, #args
    Var(Option<i32>), // optional initial value
}

#[derive(Debug)]
pub struct Context<'a> {
    pub globals: HashMap<&'a str, NameStatus>,
    pub vars: Vec<HashMap<&'a str, i32>>,
    pub break_continue: Vec<(u32, u32)>,
    pub label: u32,
    pub depth: u32,
}

impl<'a> Context<'a> {
    pub fn name_resolution(&self, name: &'a str) -> Option<i32> {
        for mp in self.vars.iter().rev() {
            if let Some(&x) = mp.get(name) {
                return Some(x);
            }
        }
        None
    }
}

#[derive(Debug)]
pub enum IrStmt {
    Const(i32),
    Unary(UnaryOp),
    Binary(BinaryOp),
    Ret,
    FrameAddr(i32),
    GlobalAddr(String),
    Load,
    Store,
    Pop,
    Label(u32),
    Bnez(u32),
    Beqz(u32),
    Br(u32),
    Call(String, usize),
}

impl<'a> Context<'a> {
    pub fn new() -> Context<'a> {
        Context {
            globals: HashMap::new(),
            vars: vec![],
            break_continue: vec![],
            label: 0,
            depth: 0,
        }
    }
}

pub fn ast2ir<'a>(p: &'a Prog<'a>) -> IrProg<'a> {
    let mut ctx = Context::new();
    let mut irfunc: Vec<IrFunc> = vec![];
    let mut global_initialized: Vec<(&str, i32)> = vec![];
    let mut global_uninitialized: Vec<&str> = vec![];
    let mut has_main = false;
    for (id, x) in p.contents.iter().enumerate() {
        match x {
            FuncDecl::Func(f) => {
                if f.name == "main" { has_main = true; }
                match f.stmts {
                    None => {
                        if let Some(_) = ctx.globals.get(f.name) {
                            panic!("Redefinition of name {} as a function!", f.name);
                        }
                        ctx.globals.insert(f.name, NameStatus::FuncDeclared(id, f.params.len()));
                    }
                    Some(_) => {
                        match ctx.globals.get(f.name) {
                            Some(NameStatus::FuncDeclared(num, expected)) => {
                                if f.params.len() != *expected {
                                    panic!("Function {} is declared to have {} parameters in #{}, but has {} parameters when implemented in #{}", f.name, expected, num, f.params.len(), id);
                                }
                            }
                            None => {}
                            _ => { panic!("Name {} is already declared/implemented!", f.name); }
                        }
                        ctx.globals.insert(f.name, NameStatus::FuncImplemented(id, f.params.len()));
                        irfunc.push(func(f, &mut ctx));
                    }
                }
            }
            FuncDecl::Decl(d) => {
                if ctx.globals.contains_key(d.name) { panic!("Redefinition of global variable {}", d.name) }
                match &d.val {
                    None => {
                        ctx.globals.insert(d.name, NameStatus::Var(None));
                        global_uninitialized.push(d.name);
                    }
                    Some(x) => {
                        let ret = const_expr(x);
                        if let Err(()) = ret { panic!("Global variable {} is not initialized to a constant expression!", d.name) }
                        ctx.globals.insert(d.name, NameStatus::Var(Some(ret.unwrap())));
                        global_initialized.push((d.name, ret.unwrap()));
                    }
                }
            }
        }
    }
    if !has_main { panic!("No main function!") }
    IrProg { funcs: irfunc, global_initialized, global_uninitialized }
}

fn func<'a>(f: &Func<'a>, ctx: &mut Context<'a>) -> IrFunc<'a> {
    let mut stmts = Vec::new();
    let mut args: HashMap<&'a str, i32> = HashMap::new();
    for (id, decl) in f.params.iter().enumerate() {
        if args.contains_key(decl.name) {
            panic!("Argument name {} appears twice in function {}", decl.name, f.name);
        }
        args.insert(decl.name, -(id as i32) - 1);
    }
    ctx.vars.push(args);
    let locals: u32 = compound(&mut stmts, ctx, f.stmts.as_ref().unwrap());
    ctx.vars.pop();
    IrFunc { name: f.name, stmts, locals }
}

fn statement<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, st: &Stmt<'a>) -> u32 {
    match st {
        Stmt::Ret(r) => {
            expr(stmts, ctx, &r);
            stmts.push(IrStmt::Ret);
            ctx.depth
        }
        Stmt::MaybeExpr(e) => {
            if let Some(x) = e {
                expr(stmts, ctx, &x);
                stmts.push(IrStmt::Pop);
            }
            ctx.depth
        }
        Stmt::If(cond, if_expr, op) => {
            expr(stmts, ctx, cond);
            let no = ctx.label;
            let mut max_depth: u32 = ctx.depth;
            if let Some(x) = op {
                ctx.label = no + 2;
                stmts.push(IrStmt::Beqz(no));
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, &*if_expr));
                stmts.push(IrStmt::Br(no + 1));
                stmts.push(IrStmt::Label(no));
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, &*x));
                stmts.push(IrStmt::Label(no + 1));
            } else {
                ctx.label = no + 1;
                stmts.push(IrStmt::Beqz(no));
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, &*if_expr));
                stmts.push(IrStmt::Label(no));
            }
            max_depth
        }
        Stmt::Compound(com) => {
            compound(stmts, ctx, &*com)
        }
        Stmt::For(cond, body, update) => {
            let no = ctx.label;
            ctx.label = no + 3;
            ctx.break_continue.push((no + 2, no));
            if let Some(x) = cond {
                expr(stmts, ctx, &*x);
                stmts.push(IrStmt::Beqz(no + 2));
            }
            stmts.push(IrStmt::Label(no + 1));
            let ret = statement(stmts, ctx, &**body);
            stmts.push(IrStmt::Label(no)); // continue
            if let Some(x) = update {
                expr(stmts, ctx, &*x);
                stmts.push(IrStmt::Pop);
            }
            if let Some(x) = cond {
                expr(stmts, ctx, &*x);
                stmts.push(IrStmt::Bnez(no + 1));
            } else {
                stmts.push(IrStmt::Br(no + 1));
            }
            stmts.push(IrStmt::Label(no + 2)); // break
            ctx.break_continue.pop();
            ret
        }
        Stmt::DoWhile(body, cond) => {
            let no = ctx.label;
            ctx.label = no + 3;
            ctx.break_continue.push((no + 2, no + 1));
            stmts.push(IrStmt::Label(no));
            let ret = statement(stmts, ctx, &**body);
            stmts.push(IrStmt::Label(no + 1)); // continue
            expr(stmts, ctx, cond);
            stmts.push(IrStmt::Bnez(no));
            stmts.push(IrStmt::Label(no + 2)); // break
            ctx.break_continue.pop();
            ret
        }
        Stmt::Break => {
            match ctx.break_continue.last() {
                Some((break_addr, _)) => {
                    stmts.push(IrStmt::Br(*break_addr))
                }
                None => {
                    panic!("Invalid break!")
                }
            }
            ctx.depth
        }
        Stmt::Continue => {
            match ctx.break_continue.last() {
                Some((_, continue_addr)) => {
                    stmts.push(IrStmt::Br(*continue_addr))
                }
                None => {
                    panic!("Invalid continue!")
                }
            }
            ctx.depth
        }
    }
}

fn compound<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, com: &Vec<BlockItem<'a>>) -> u32 {
    let mut max_depth = ctx.depth;
    ctx.vars.push(HashMap::new());
    for x in com {
        match x {
            BlockItem::Stmt(st) => {
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, st));
            }
            BlockItem::Decl(e) => {
                if ctx.vars.last().unwrap().contains_key(e.name) {
                    panic!("Variable {} is defined twice", e.name);
                } else if ctx.vars.len() == 2 && ctx.vars[0].contains_key(e.name) {
                    panic!("Variable {} is defined as function parameter", e.name);
                } else {
                    let id = ctx.depth;
                    ctx.depth += 1;
                    ctx.vars.last_mut().unwrap().insert(e.name, id as i32);
                    if let Some(x) = &e.val {
                        expr(stmts, ctx, &x);
                        stmts.push(IrStmt::FrameAddr(*ctx.vars.last().unwrap().get(e.name).unwrap()));
                        stmts.push(IrStmt::Store);
                        stmts.push(IrStmt::Pop);
                    }
                }
                max_depth = std::cmp::max(max_depth, ctx.depth);
            }
        }
    }
    ctx.depth -= ctx.vars.last().unwrap().len() as u32;
    ctx.vars.pop();
    max_depth
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
            if let Some(x) = ctx.name_resolution(name) {
                expr(stmts, ctx, &**val);
                stmts.push(IrStmt::FrameAddr(x));
                stmts.push(IrStmt::Store);
            } else if let Some(NameStatus::Var(_)) = ctx.globals.get(name) {
                expr(stmts, ctx, &**val);
                stmts.push(IrStmt::GlobalAddr(String::from(*name)));
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
        Unary::Call(name, params) => {
            match ctx.globals.get(*name) {
                None => { panic!("Function {} is used before declaration/implementation!", *name); }
                Some(NameStatus::Var(_)) => { panic!("Global variable {} used as a function!", *name); }
                Some(NameStatus::FuncDeclared(_, expected)) => {
                    if *expected != params.len() {
                        panic!("Function {} requires {} parameters, but {} is given!", *name, expected, params.len());
                    }
                }
                Some(NameStatus::FuncImplemented(_, expected)) => {
                    if *expected != params.len() {
                        panic!("Function {} requires {} parameters, but {} is given!", *name, expected, params.len());
                    }
                }
            }
            for arg in params.iter().rev() {
                expr(stmts, ctx, arg);
            }
            stmts.push(IrStmt::Call(String::from(*name), params.len()));
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
            match ctx.name_resolution(name) {
                Some(x) => {
                    stmts.push(IrStmt::FrameAddr(x));
                    stmts.push(IrStmt::Load);
                }
                None => {
                    if let Some(NameStatus::Var(_)) = ctx.globals.get(name) {
                        stmts.push(IrStmt::GlobalAddr(String::from(*name)));
                        stmts.push(IrStmt::Load);
                    } else {
                        panic!("Name {} is used before declaration!", name);
                    }
                }
            }
        }
    }
}

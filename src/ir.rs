use std::collections::HashMap;

use crate::ast::*;
use crate::consteval::const_expr;

#[derive(Debug)]
pub struct IrProg<'a> {
    pub funcs: Vec<IrFunc<'a>>,
    pub globals: Vec<(&'a str, i32)>,
}

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub name: &'a str,
    pub stmts: Vec<IrStmt>,
    pub locals: u32,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum NameStatus {
    FuncDeclared(FunctionSignature),
    FuncImplemented(FunctionSignature),
    Var(Type),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunctionSignature {
    pub ret: Type,
    pub args: Vec<Type>,
}

impl FunctionSignature {
    pub fn from(f: &Func) -> FunctionSignature {
        FunctionSignature {
            ret: f.ret,
            args: f.params.iter().map(|x| x.typ).collect(),
        }
    }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct VarInfo {
    pub addr: i32,
    pub typ: Type,
}

#[derive(Debug)]
pub struct Context<'a> {
    pub globals: HashMap<&'a str, NameStatus>,
    pub vars: Vec<HashMap<&'a str, VarInfo>>,
    pub break_continue: Vec<(u32, u32)>,
    pub label: u32,
    pub depth: u32,
}

impl<'a> Context<'a> {
    pub fn name_resolution(&self, name: &'a str) -> Option<(IrStmt, Type)> {
        for mp in self.vars.iter().rev() {
            if let Some(&x) = mp.get(name) {
                return Some((IrStmt::FrameAddr(x.addr), x.typ));
            }
        }
        if let Some(NameStatus::Var(typ)) = self.globals.get(name) {
            return Some((IrStmt::GlobalAddr(String::from(name)), *typ))
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
    let mut globals: Vec<(&str, i32)> = vec![];
    let mut has_main = false;
    for x in p.contents.iter() {
        match x {
            FuncDecl::Func(f) => {
                if f.name == "main" { has_main = true; }
                let sig = FunctionSignature::from(f);
                match f.stmts {
                    None => {
                        if let Some(_) = ctx.globals.get(f.name) {
                            panic!("Redefinition of name {} as a function!", f.name);
                        }
                        ctx.globals.insert(f.name, NameStatus::FuncDeclared(sig));
                    }
                    Some(_) => {
                        match ctx.globals.get(f.name) {
                            Some(NameStatus::FuncDeclared(expected)) => {
                                if sig != *expected {
                                    panic!("Function {} signature is {:?}, but was previously declared as {:?}", f.name, sig, expected);
                                }
                            }
                            None => {}
                            _ => { panic!("Name {} is already declared/implemented!", f.name); }
                        }
                        ctx.globals.insert(f.name, NameStatus::FuncImplemented(sig));
                        irfunc.push(func(f, &mut ctx));
                    }
                }
            }
            FuncDecl::Decl(d) => {
                if ctx.globals.contains_key(d.name) { panic!("Redefinition of global variable {}", d.name) }
                match &d.val {
                    None => {
                        ctx.globals.insert(d.name, NameStatus::Var(d.typ));
                        globals.push((d.name, 0));
                    }
                    Some(x) => {
                        let result = const_expr(x).expect(&format!("Global variable {} is not initialized to a constant expression!", d.name));
                        if d.typ != result.typ { panic!("Initializing {} with an expression of type 'int'", d.typ); }
                        ctx.globals.insert(d.name, NameStatus::Var(d.typ));
                        globals.push((d.name, result.val));
                    }
                }
            }
        }
    }
    if !has_main { panic!("No main function!") }
    IrProg { funcs: irfunc, globals }
}

fn func<'a>(f: &Func<'a>, ctx: &mut Context<'a>) -> IrFunc<'a> {
    let mut stmts = Vec::new();
    let mut args: HashMap<&'a str, VarInfo> = HashMap::new();
    for (id, decl) in f.params.iter().enumerate() {
        if args.contains_key(decl.name) {
            panic!("Argument name {} appears twice in function {}", decl.name, f.name);
        }
        args.insert(decl.name, VarInfo { addr: -(id as i32) - 1, typ: decl.typ });
    }
    ctx.vars.push(args);
    let locals: u32 = compound(&mut stmts, ctx, f.stmts.as_ref().unwrap(), f.ret);
    ctx.vars.pop();
    IrFunc { name: f.name, stmts, locals }
}

fn statement<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, st: &Stmt<'a>, return_type: Type) -> u32 {
    match st {
        Stmt::Ret(r) => {
            let typ = expr(stmts, ctx, &r, false);
            if typ != return_type { panic!("Expect return type {}, but {} provided", return_type, typ) }
            stmts.push(IrStmt::Ret);
            ctx.depth
        }
        Stmt::MaybeExpr(e) => {
            if let Some(x) = e {
                expr(stmts, ctx, &x, false);
                stmts.push(IrStmt::Pop);
            }
            ctx.depth
        }
        Stmt::If(cond, if_expr, op) => {
            let condition_type = expr(stmts, ctx, cond, false);
            if condition_type.cnt > 0 { panic!("Cannot use {} as condition in {:?}", condition_type, st) }
            let no = ctx.label;
            let mut max_depth: u32 = ctx.depth;
            if let Some(x) = op {
                ctx.label = no + 2;
                stmts.push(IrStmt::Beqz(no));
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, &*if_expr, return_type));
                stmts.push(IrStmt::Br(no + 1));
                stmts.push(IrStmt::Label(no));
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, &*x, return_type));
                stmts.push(IrStmt::Label(no + 1));
            } else {
                ctx.label = no + 1;
                stmts.push(IrStmt::Beqz(no));
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, &*if_expr, return_type));
                stmts.push(IrStmt::Label(no));
            }
            max_depth
        }
        Stmt::Compound(com) => {
            compound(stmts, ctx, &*com, return_type)
        }
        Stmt::For(cond, body, update) => {
            let no = ctx.label;
            ctx.label = no + 3;
            ctx.break_continue.push((no + 2, no));
            if let Some(x) = cond {
                let typ = expr(stmts, ctx, &*x, false);
                if typ.cnt > 0 {
                    panic!("Type {} cannot be used as condition in {:?}.", typ, st)
                }
                stmts.push(IrStmt::Beqz(no + 2));
            }
            stmts.push(IrStmt::Label(no + 1));
            let ret = statement(stmts, ctx, &**body, return_type);
            stmts.push(IrStmt::Label(no)); // continue
            if let Some(x) = update {
                expr(stmts, ctx, &*x, false);
                stmts.push(IrStmt::Pop);
            }
            if let Some(x) = cond {
                expr(stmts, ctx, &*x, false);
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
            let ret = statement(stmts, ctx, &**body, return_type);
            stmts.push(IrStmt::Label(no + 1)); // continue
            let condition_type = expr(stmts, ctx, cond, false);
            if condition_type.cnt > 0 { panic!("Cannot use {} as condition in {:?}", condition_type, st) }
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

fn compound<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, com: &Vec<BlockItem<'a>>, return_type: Type) -> u32 {
    let mut max_depth = ctx.depth;
    ctx.vars.push(HashMap::new());
    for x in com {
        match x {
            BlockItem::Stmt(st) => {
                max_depth = std::cmp::max(max_depth, statement(stmts, ctx, st, return_type));
            }
            BlockItem::Decl(e) => {
                if ctx.vars.last().unwrap().contains_key(e.name) {
                    panic!("Variable {} is defined twice", e.name);
                } else if ctx.vars.len() == 2 && ctx.vars[0].contains_key(e.name) {
                    panic!("Variable {} is defined as function parameter", e.name);
                } else {
                    ctx.vars.last_mut().unwrap().insert(e.name, VarInfo { addr: ctx.depth as i32, typ: e.typ });
                    ctx.depth += 1;
                    if let Some(x) = &e.val {
                        let rhs_type = expr(stmts, ctx, &x, false);
                        if rhs_type != e.typ { panic!("Invalid assignment in {:?}: {} <- {}.", e, e.typ, rhs_type) }
                        stmts.push(IrStmt::FrameAddr(ctx.vars.last().unwrap().get(e.name).unwrap().addr));
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

fn conditional<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, o: &Conditional, lvalue: bool) -> Type {
    match o {
        Conditional::LOr(a) => { logical_or(stmts, ctx, &a, lvalue) }
        Conditional::Cond(cond, t, f) => {
            let no: u32 = ctx.label;
            ctx.label = no + 2;
            let cond_type = logical_or(stmts, ctx, &cond, false);
            if cond_type.cnt > 0 { panic!("Expression {:?} Used type {} where boolean type is required", cond, cond_type) }
            stmts.push(IrStmt::Beqz(no));
            let t_type = expr(stmts, ctx, &t, lvalue);
            stmts.push(IrStmt::Br(no + 1));
            stmts.push(IrStmt::Label(no));
            let f_type = conditional(stmts, ctx, f, lvalue);
            stmts.push(IrStmt::Label(no + 1));
            if t_type != f_type { panic!("Type mismatch in conditional expression ({:?} and {:?})", t_type, f_type) }
            t_type
        }
    }
}

fn expr<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &Expr, lvalue: bool) -> Type {
    match e {
        Expr::Assign(u, val) => {
            if lvalue { panic!("Result of assign expression {:?} cannot be used as a lvalue!", e) }
            let rtype = expr(stmts, ctx, &**val, false);
            let ltype = unary(stmts, ctx, u, true);
            if ltype != rtype { panic!("Operands of assign expression do not have the same type: {} and {}", ltype, rtype) }
            stmts.push(IrStmt::Store);
            ltype
        }
        Expr::Cond(a) => {
            conditional(stmts, ctx, a, lvalue)
        }
    }
}

fn logical_or<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &LogicalOr, lvalue: bool) -> Type {
    match e {
        LogicalOr::LAnd(a) => { logical_and(stmts, ctx, a, lvalue) }
        LogicalOr::Bop(a, op, b) => {
            let x = logical_or(stmts, ctx, &**a, lvalue);
            let y = logical_and(stmts, ctx, b, lvalue);
            stmts.push(IrStmt::Binary(*op));
            let (typ, rv) = binary_operation(*op, (x, lvalue), (y, lvalue));
            if rv < lvalue { panic!("Binary expression {:?} cannot be used as a lvalue!", e); }
            typ
        }
    }
}

fn logical_and<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &LogicalAnd, lvalue: bool) -> Type {
    match e {
        LogicalAnd::Eqn(a) => { equality(stmts, ctx, a, lvalue) }
        LogicalAnd::Bop(a, op, b) => {
            let x = logical_and(stmts, ctx, &**a, lvalue);
            let y = equality(stmts, ctx, b, lvalue);
            stmts.push(IrStmt::Binary(*op));
            let (typ, rv) = binary_operation(*op, (x, lvalue), (y, lvalue));
            if rv < lvalue { panic!("Binary expression {:?} cannot be used as a lvalue!", e); }
            typ
        }
    }
}

fn equality<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &Equality, lvalue: bool) -> Type {
    match e {
        Equality::Rel(a) => { relational(stmts, ctx, a, lvalue) }
        Equality::Bop(a, op, b) => {
            let x = equality(stmts, ctx, &**a, lvalue);
            let y = relational(stmts, ctx, b, lvalue);
            stmts.push(IrStmt::Binary(*op));
            let (typ, rv) = binary_operation(*op, (x, lvalue), (y, lvalue));
            if rv < lvalue { panic!("Binary expression {:?} cannot be used as a lvalue!", e); }
            typ
        }
    }
}

fn relational<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, e: &Relational, lvalue: bool) -> Type {
    match e {
        Relational::Add(a) => { additive(stmts, ctx, a, lvalue) }
        Relational::Bop(a, op, b) => {
            let x = relational(stmts, ctx, &**a, lvalue);
            let y = additive(stmts, ctx, b, lvalue);
            stmts.push(IrStmt::Binary(*op));
            let (typ, rv) = binary_operation(*op, (x, lvalue), (y, lvalue));
            if rv < lvalue { panic!("Binary expression {:?} cannot be used as a lvalue!", e); }
            typ
        }
    }
}

fn additive<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, a: &Additive, lvalue: bool) -> Type {
    match a {
        Additive::Mul(m) => { multiplicative(stmts, ctx, m, lvalue) }
        Additive::Bop(a, op, b) => {
            let x = additive(stmts, ctx, &**a, lvalue);
            let y = multiplicative(stmts, ctx, b, lvalue);
            stmts.push(IrStmt::Binary(*op));
            let (typ, rv) = binary_operation(*op, (x, lvalue), (y, lvalue));
            if rv < lvalue { panic!("Binary expression {:?} cannot be used as a lvalue!", a); }
            typ
        }
    }
}

fn multiplicative<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, m: &Multiplicative, lvalue: bool) -> Type {
    match m {
        Multiplicative::U(u) => { unary(stmts, ctx, u, lvalue) }
        Multiplicative::Mul(m, op, a) => {
            let x = multiplicative(stmts, ctx, &**m, lvalue);
            let y = unary(stmts, ctx, a, lvalue);
            stmts.push(IrStmt::Binary(*op));
            let (typ, rv) = binary_operation(*op, (x, lvalue), (y, lvalue));
            if rv < lvalue { panic!("Binary expression {:?} cannot be used as a lvalue!", m); }
            typ
        }
    }
}

fn unary<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, u: &Unary, lvalue: bool) -> Type {
    match u {
        Unary::Prim(p) => { primary(stmts, ctx, p, lvalue) }
        Unary::Uop(op, v) => {
            if *op == UnaryOp::Ref {
                if lvalue { panic!("Reference expression {:?} cannot be used as a lvalue!", u); }
                let ret = unary(stmts, ctx, v, true);
                Type { cnt: ret.cnt + 1 }
            } else {
                let ret = unary(stmts, ctx, v, if *op == UnaryOp::Deref { false } else { lvalue });
                if !lvalue || *op != UnaryOp::Deref { stmts.push(IrStmt::Unary(*op)); }
                let (typ, rv) = unary_operation(*op, (ret, lvalue));
                if rv < lvalue { panic!("Unary expression {:?} cannot be used as a lvalue!", u); }
                typ
            }
        }
        Unary::Call(name, params) => {
            if lvalue { panic!("Function call {:?} cannot be used as a lvalue!", u); }
            let args_type: Vec<Type> = params.iter().rev().map(|x| expr(stmts, ctx, x, false)).collect();
            match ctx.globals.get(*name) {
                None => { panic!("Function {} is used before declaration/implementation!", *name); }
                Some(NameStatus::Var(_)) => { panic!("Global variable {} used as a function!", *name); }
                Some(NameStatus::FuncDeclared(expected)) => {
                    if args_type != expected.args {
                        panic!("Function {} requires parameters of type {:?}, but {:?} is given!", *name, expected.args, args_type);
                    }
                    stmts.push(IrStmt::Call(String::from(*name), params.len()));
                    expected.ret
                }
                Some(NameStatus::FuncImplemented(expected)) => {
                    if args_type != expected.args {
                        panic!("Function {} requires parameters of type {:?}, but {:?} is given!", *name, expected.args, args_type);
                    }
                    stmts.push(IrStmt::Call(String::from(*name), params.len()));
                    expected.ret
                }
            }
        }
        Unary::ExplicitConversion(typ, x) => {
            let _ = unary(stmts, ctx, x, lvalue);
            *typ
        }
    }
}

fn primary<'a>(stmts: &mut Vec<IrStmt>, ctx: &mut Context<'a>, p: &Primary, lvalue: bool) -> Type {
    match p {
        Primary::Int(i, _) => {
            if lvalue { panic!("Integer {} cannot be used as a lvalue", i); }
            stmts.push(IrStmt::Const(*i));
            Type { cnt: 0 }
        }
        Primary::Braced(e) => {
            expr(stmts, ctx, e, lvalue)
        }
        Primary::Identifier(name) => {
            match ctx.name_resolution(name) {
                Some((ir, typ)) => {
                    stmts.push(ir);
                    if !lvalue { stmts.push(IrStmt::Load); }
                    typ
                }
                None => {
                    panic!("Name {} is used before declaration!", name);
                }
            }
        }
    }
}
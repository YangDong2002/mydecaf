use crate::ast::*;

#[derive(Debug)]
pub struct IrProg<'a> {
    pub func: IrFunc<'a>
}

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub name: &'a str,
    pub stmts: Vec<IrStmt>
}

#[derive(Debug)]
pub enum IrStmt {
    Const(i32),
	Unary(UnaryOp),
    Binary(BinaryOp),
    Ret
}

pub fn ast2ir<'a>(p: &'a Prog<'a>) -> IrProg<'a> {
    IrProg { func: func(&p.func) }
}
fn func<'a>(f: &Func<'a>) -> IrFunc<'a> {
    let mut stmts = Vec::new();
    match &f.stmt {
        Stmt::Ret(e) => {
            expr(&mut stmts, e);
            stmts.push(IrStmt::Ret);
        }
    }
    IrFunc { name: f.name, stmts }
}
fn expr(stmts: &mut Vec<IrStmt>, e: &Expr) {
    match e {
        Expr::Add(a) => { additive(stmts, a); }
    }
}
fn additive(stmts: &mut Vec<IrStmt>, a: &Additive) {
    match a {
        Additive::Mul(m) => { multiplicative(stmts, m); },
        Additive::Bop(a, op, b) => {
            additive(stmts, &**a);
            multiplicative(stmts, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}
fn multiplicative(stmts: &mut Vec<IrStmt>, m: &Multiplicative) {
    match m {
        Multiplicative::U(u) => unary(stmts, u),
        Multiplicative::Mul(m, op, a) => {
            multiplicative(stmts, &**m);
            unary(stmts, a);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}
fn unary(stmts: &mut Vec<IrStmt>, u: &Unary) {
    match u {
        Unary::Prim(p) => primary(stmts, p),
        Unary::Uop(op, v) => {
            unary(stmts, v);
            stmts.push(IrStmt::Unary(*op));
        }
    }
}
fn primary(stmts: &mut Vec<IrStmt>, p: &Primary) {
    match p {
        Primary::Int(i, _) => {
            stmts.push(IrStmt::Const(*i));
        }
        Primary::Braced(e) => {
            expr(stmts, e);
        }
    }
}

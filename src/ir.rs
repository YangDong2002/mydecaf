use crate::ast::*;

#[derive(Debug)]
pub struct IrProg<'a> {
    pub func: IrFunc<'a>
}

#[derive(Debug)]
pub struct IrFunc<'a> {
    pub name: &'a str,
    pub stmts: Vec<IrStmt>,
}

#[derive(Debug)]
pub enum IrStmt {
    Const(i32),
    Unary(UnaryOp),
    Binary(BinaryOp),
    Ret,
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
        Expr::LOr(a) => { logical_or(stmts, a); }
    }
}

fn logical_or(stmts: &mut Vec<IrStmt>, e: &LogicalOr) {
    match e {
        LogicalOr::LAnd(a) => { logical_and(stmts, a); }
        LogicalOr::Bop(a, op, b) => {
            logical_or(stmts, &**a);
            logical_and(stmts, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn logical_and(stmts: &mut Vec<IrStmt>, e: &LogicalAnd) {
    match e {
        LogicalAnd::Eqn(a) => { equality(stmts, a); }
        LogicalAnd::Bop(a, op, b) => {
            logical_and(stmts, &**a);
            equality(stmts, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn equality(stmts: &mut Vec<IrStmt>, e: &Equality) {
    match e {
        Equality::Rel(a) => { relational(stmts, a); }
        Equality::Bop(a, op, b) => {
            equality(stmts, &**a);
            relational(stmts, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn relational(stmts: &mut Vec<IrStmt>, e: &Relational) {
    match e {
        Relational::Add(a) => { additive(stmts, a); }
        Relational::Bop(a, op, b) => {
            relational(stmts, &**a);
            additive(stmts, b);
            stmts.push(IrStmt::Binary(*op));
        }
    }
}

fn additive(stmts: &mut Vec<IrStmt>, a: &Additive) {
    match a {
        Additive::Mul(m) => { multiplicative(stmts, m); }
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

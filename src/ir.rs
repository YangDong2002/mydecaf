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
        Expr::Int(x, _) => stmts.push(IrStmt::Const(*x))
    }
}


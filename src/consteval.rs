use crate::ast::*;

pub fn const_expr(e: &Expr) -> Result<i32, ()> {
    match e {
        Expr::Assign(_, _) => { Err(()) }
        Expr::Cond(x) => { const_cond(x) }
    }
}

pub fn const_cond(c: &Conditional) -> Result<i32, ()> {
    match c {
        Conditional::Cond(cond, t, f) => {
            if const_lor(cond)? != 0 { const_expr(&*t) } else { const_cond(&*f) }
        }
        Conditional::LOr(l) => { const_lor(l) }
    }
}

pub fn const_lor(l: &LogicalOr) -> Result<i32, ()> {
    match l {
        LogicalOr::Bop(x, op, y) => {
            let a = const_lor(&*x)?;
            let b = const_land(&*y)?;
            if let BinaryOp::LOr = op {
                return Ok(((a != 0) || (b != 0)) as i32);
            }
            unreachable!();
        }
        LogicalOr::LAnd(x) => { const_land(x) }
    }
}

pub fn const_land(l: &LogicalAnd) -> Result<i32, ()> {
    match l {
        LogicalAnd::Eqn(a) => { const_eqn(a) }
        LogicalAnd::Bop(x, op, y) => {
            let a = const_land(&*x)?;
            let b = const_eqn(y)?;
            if let BinaryOp::LAnd = op {
                return Ok(((a != 0) && (b != 0)) as i32);
            }
            panic!("Impossible!");
        }
    }
}

pub fn const_eqn(e: &Equality) -> Result<i32, ()> {
    match e {
        Equality::Rel(x) => { const_rel(x) }
        Equality::Bop(x, op, y) => {
            let a = const_eqn(&*x)?;
            let b = const_rel(y)?;
            if let BinaryOp::Eqt = op {
                return Ok((a == b) as i32);
            }
            if let BinaryOp::Neq = op {
                return Ok((a != b) as i32);
            }
            unreachable!()
        }
    }
}

pub fn const_rel(r: &Relational) -> Result<i32, ()> {
    match r {
        Relational::Add(x) => { const_add(x) }
        Relational::Bop(x, op, y) => {
            let a = const_rel(&*x)?;
            let b = const_add(y)?;
            match op {
                BinaryOp::Geq => { Ok((a >= b) as i32) }
                BinaryOp::Gt => { Ok((a > b) as i32) }
                BinaryOp::Leq => { Ok((a <= b) as i32) }
                BinaryOp::Lt => { Ok((a < b) as i32) }
                _ => { unreachable!() }
            }
        }
    }
}

pub fn const_add(r: &Additive) -> Result<i32, ()> {
    match r {
        Additive::Mul(x) => { const_mul(x) }
        Additive::Bop(x, op, y) => {
            let a = const_add(&*x)?;
            let b = const_mul(y)?;
            match op {
                BinaryOp::Add => { Ok(a + b) }
                BinaryOp::Sub => { Ok(a - b) }
                _ => { unreachable!() }
            }
        }
    }
}

pub fn const_mul(r: &Multiplicative) -> Result<i32, ()> {
    match r {
        Multiplicative::U(x) => { const_unary(x) }
        Multiplicative::Mul(x, op, y) => {
            let a = const_mul(&*x)?;
            let b = const_unary(y)?;
            match op {
                BinaryOp::Mul => { Ok(a * b) }
                BinaryOp::Div => { Ok(a / b) }
                BinaryOp::Mod => { Ok(a % b) }
                _ => { unreachable!() }
            }
        }
    }
}

pub fn const_unary(r: &Unary) -> Result<i32, ()> {
    match r {
        Unary::Call(_, _) => { Err(()) }
        Unary::Uop(op, y) => {
            let a = const_unary(&*y)?;
            match op {
                UnaryOp::UBNot => { Ok(!a) }
                UnaryOp::UNot => { Ok(if a != 0 { 1 } else { 0 }) }
                UnaryOp::UNeg => { Ok(-a) }
            }
        }
        Unary::Prim(p) => { const_primary(p) }
    }
}

pub fn const_primary(p: &Primary) -> Result<i32, ()> {
    match p {
        Primary::Int(x, _) => { Ok(*x) }
        Primary::Braced(x) => { const_expr(&*x) }
        Primary::Identifier(_) => { Err(()) }
    }
}
use crate::ast::*;

pub struct ConstResult {
    pub val: i32,
    pub typ: Type,
}

pub fn const_expr(e: &Expr) -> Result<ConstResult, ()> {
    match e {
        Expr::Assign(_, _) => { Err(()) }
        Expr::Cond(x) => { const_cond(x) }
    }
}

pub fn const_cond(c: &Conditional) -> Result<ConstResult, ()> {
    match c {
        Conditional::Cond(cond, t, f) => {
            let cond_val = const_lor(cond)?;
            if cond_val.typ.cnt != 0 { panic!("Type {} cannot be used as condition in constant propagation!", cond_val.typ) }
            if cond_val.val != 0 { const_expr(&*t) } else { const_cond(&*f) }
        }
        Conditional::LOr(l) => { const_lor(l) }
    }
}

pub fn const_lor(l: &LogicalOr) -> Result<ConstResult, ()> {
    match l {
        LogicalOr::Bop(x, op, y) => {
            let a = const_lor(&*x)?;
            let b = const_land(&*y)?;
            if let BinaryOp::LOr = op {
                if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand || for type {} {} in constant propagation!", a.typ, b.typ) }
                return Ok(ConstResult { val: ((a.val != 0) || (b.val != 0)) as i32, typ: a.typ });
            }
            unreachable!();
        }
        LogicalOr::LAnd(x) => { const_land(x) }
    }
}

pub fn const_land(l: &LogicalAnd) -> Result<ConstResult, ()> {
    match l {
        LogicalAnd::Eqn(a) => { const_eqn(a) }
        LogicalAnd::Bop(x, op, y) => {
            let a = const_land(&*x)?;
            let b = const_eqn(y)?;
            if let BinaryOp::LAnd = op {
                if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand && for type {} {} in constant propagation!", a.typ, b.typ) }
                return Ok(ConstResult { val: ((a.val != 0) && (b.val != 0)) as i32, typ: a.typ });
            }
            panic!("Impossible!");
        }
    }
}

pub fn const_eqn(e: &Equality) -> Result<ConstResult, ()> {
    match e {
        Equality::Rel(x) => { const_rel(x) }
        Equality::Bop(x, op, y) => {
            let a = const_eqn(&*x)?;
            let b = const_rel(y)?;
            if let BinaryOp::Eqt = op {
                if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand == for type {} {} in constant propagation!", a.typ, b.typ) }
                return Ok(ConstResult { val: (a.val == b.val) as i32, typ: a.typ });
            }
            if let BinaryOp::Neq = op {
                if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand != for type {} {} in constant propagation!", a.typ, b.typ) }
                return Ok(ConstResult { val: (a.val != b.val) as i32, typ: a.typ });
            }
            unreachable!()
        }
    }
}

pub fn const_rel(r: &Relational) -> Result<ConstResult, ()> {
    match r {
        Relational::Add(x) => { const_add(x) }
        Relational::Bop(x, op, y) => {
            let a = const_rel(&*x)?;
            let b = const_add(y)?;
            if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand {:?} for type {} {} in constant propagation!", op, a.typ, b.typ) }
            match op {
                BinaryOp::Geq => { Ok(ConstResult { val: (a.val >= b.val) as i32, typ: a.typ }) }
                BinaryOp::Gt => { Ok(ConstResult { val: (a.val > b.val) as i32, typ: a.typ }) }
                BinaryOp::Leq => { Ok(ConstResult { val: (a.val <= b.val) as i32, typ: a.typ }) }
                BinaryOp::Lt => { Ok(ConstResult { val: (a.val < b.val) as i32, typ: a.typ }) }
                _ => { unreachable!() }
            }
        }
    }
}

pub fn const_add(r: &Additive) -> Result<ConstResult, ()> {
    match r {
        Additive::Mul(x) => { const_mul(x) }
        Additive::Bop(x, op, y) => {
            let a = const_add(&*x)?;
            let b = const_mul(y)?;
            if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand {:?} for type {} {} in constant propagation!", op, a.typ, b.typ) }

            match op {
                BinaryOp::Add => { Ok(ConstResult { val: (a.val + b.val) as i32, typ: a.typ }) }
                BinaryOp::Sub => { Ok(ConstResult { val: (a.val - b.val) as i32, typ: a.typ }) }
                _ => { unreachable!() }
            }
        }
    }
}

pub fn const_mul(r: &Multiplicative) -> Result<ConstResult, ()> {
    match r {
        Multiplicative::U(x) => { const_unary(x) }
        Multiplicative::Mul(x, op, y) => {
            let a = const_mul(&*x)?;
            let b = const_unary(y)?;
            if a.typ.cnt > 0 || b.typ.cnt > 0 { panic!("No matching operand {:?} for type {} {} in constant propagation!", op, a.typ, b.typ) }

            match op {
                BinaryOp::Mul => { Ok(ConstResult { val: (a.val * b.val) as i32, typ: a.typ }) }
                BinaryOp::Div => { Ok(ConstResult { val: (a.val / b.val) as i32, typ: a.typ }) }
                BinaryOp::Mod => { Ok(ConstResult { val: (a.val % b.val) as i32, typ: a.typ }) }
                _ => { unreachable!() }
            }
        }
    }
}

pub fn const_unary(r: &Unary) -> Result<ConstResult, ()> {
    match r {
        Unary::Call(_, _) => { Err(()) }
        Unary::Uop(op, y) => {
            let a = const_unary(&*y)?;
            if a.typ.cnt > 0 { panic!("No matching operand {:?} for type {} in constant propagation!", op, a.typ) }
            match op {
                UnaryOp::UBNot => { Ok(ConstResult { val: !a.val, typ: a.typ }) }
                UnaryOp::UNot => { Ok(ConstResult { val: if a.val != 0 { 1 } else { 0 }, typ: a.typ }) }
                UnaryOp::UNeg => { Ok(ConstResult { val: -a.val, typ: a.typ }) }
                _ => { Err(()) }
            }
        }
        Unary::Prim(p) => { const_primary(p) }
        Unary::ExplicitConversion(typ, val) => {
            Ok(ConstResult { val: const_unary(&*val)?.val, typ: *typ })
        }
    }
}

pub fn const_primary(p: &Primary) -> Result<ConstResult, ()> {
    match p {
        Primary::Int(x, _) => { Ok(ConstResult { val: *x, typ: Type { cnt: 0 } }) }
        Primary::Braced(x) => { const_expr(&*x) }
        Primary::Identifier(_) => { Err(()) }
    }
}
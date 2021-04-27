use std::io::{Result, Write};
use crate::{ir::*, ast::UnaryOp::*, ast::BinaryOp::*};
pub fn write_asm(p: &IrProg, w: &mut impl Write) -> Result<()> {
    let f = &p.func;

    for s in &f.stmts {
        eprintln!("{:?}", s);
    }
    writeln!(w, ".global {}", f.name)?;
    writeln!(w, "{}:", f.name)?;
    for s in &f.stmts {
        writeln!(w, "  # {:?}", s)?;
        match s {
            IrStmt::Const(x) => {
                writeln!(w, "  li t0, {}", x)?;
                writeln!(w, "  sw t0, -4(sp)")?;
                writeln!(w, "  add sp, sp, -4")?;
            }
			IrStmt::Unary(op) => {
				writeln!(w, "  lw t0, 0(sp)")?;
				writeln!(w, "  {} t0, t0", match op { UNeg => "neg", UNot => "seqz", UBNot => "not"})?;
				writeln!(w, "  sw t0, 0(sp)")?;
			}
            IrStmt::Binary(op) => {
                writeln!(w, "  lw t1, 4(sp)")?;
                writeln!(w, "  lw t2, 0(sp)")?;
                writeln!(w, "  {} t1, t1, t2", match op { Add => "add", Sub => "sub", Mul => "mul", Div => "div", Mod => "rem" })?;
                writeln!(w, "  addi sp, sp, 4")?;
                writeln!(w, "  sw t1, 0(sp)")?;
            }
            IrStmt::Ret => {
                writeln!(w, "  lw a0, 0(sp)")?;
                writeln!(w, "  add sp, sp, 4")?;
                writeln!(w, "  ret")?;
            }
        }
    }
    Ok(())
}

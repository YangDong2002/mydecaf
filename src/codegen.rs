use std::io::{Result, Write};
use crate::{ir::*, ast::UnaryOp::*};
pub fn write_asm(p: &IrProg, w: &mut impl Write) -> Result<()> {
    let f = &p.func;
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
            IrStmt::Ret => {
                writeln!(w, "  lw a0, 0(sp)")?;
                writeln!(w, "  add sp, sp, 4")?;
                writeln!(w, "  ret")?;
            }
        }
    }
    Ok(())
}

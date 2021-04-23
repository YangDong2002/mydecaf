use std::io::{Result, Write};
use crate::ir::*;
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
            IrStmt::Ret => {
                writeln!(w, "  lw a0, 0(sp)")?;
                writeln!(w, "  add sp, sp, 4")?;
                writeln!(w, "  ret")?;
            }
        }
    }
    Ok(())
}

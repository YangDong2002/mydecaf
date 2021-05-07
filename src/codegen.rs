use std::io::{Result, Write};

use crate::{ast::BinaryOp::*, ast::UnaryOp::*, ir::*};

pub fn write_asm(p: &IrProg, w: &mut impl Write) -> Result<()> {
    let f = &p.func;

    for s in &f.stmts {
        eprintln!("{:?}", s);
    }
    writeln!(w, ".global {}", f.name)?;
    writeln!(w, "{}:", f.name)?;

    let frame_size = (f.locals * 4 + 8) as i32;
    writeln!(w, "  addi sp, sp, {}", -frame_size)?;
    writeln!(w, "  sw ra, {}(sp)", frame_size - 4)?;
    writeln!(w, "  sw fp, {}(sp)", frame_size - 8)?;
    writeln!(w, "  addi fp, fp, {}", frame_size)?;

    for s in &f.stmts {
        writeln!(w, "  # {:?}", s)?;
        match s {
            IrStmt::Const(x) => {
                writeln!(w, "  li t0, {}", x)?;
                writeln!(w, "  sw t0, -4(sp)")?;
                writeln!(w, "  addi sp, sp, -4")?;
            }
            IrStmt::Unary(op) => {
                writeln!(w, "  lw t0, 0(sp)")?;
                writeln!(w, "  {} t0, t0", match op {
                    UNeg => "neg",
                    UNot => "seqz",
                    UBNot => "not"
                })?;
                writeln!(w, "  sw t0, 0(sp)")?;
            }
            IrStmt::Binary(op) => {
                writeln!(w, "  lw t1, 4(sp)")?;
                writeln!(w, "  lw t2, 0(sp)")?;

                writeln!(w, "{}", match op {
                    Add => "  add t1, t1, t2",
                    Sub => "  sub t1, t1, t2",
                    Mul => "  mul t1, t1, t2",
                    Div => "  div t1, t1, t2",
                    Mod => "  rem t1, t1, t2",
                    LOr => "  or t1, t1, t2\n  snez t1,t1",
                    LAnd => "  snez t1, t1\n  snez t2, t2\n  and t1, t1, t2",
                    Lt => "  slt t1, t1, t2",
                    Gt => "  slt t1, t2, t1",
                    Geq => "  slt t1, t1, t2\n  xor t1, t1, 1",
                    Leq => "  slt t1, t2, t1\n   xor t1, t1, 1",
                    Eqt => "  xor t1, t1, t2\n  seqz t1, t1",
                    Neq => "  xor t1, t1, t2\n  snez t1, t1"
                })?;
                writeln!(w, "  addi sp, sp, 4")?;
                writeln!(w, "  sw t1, 0(sp)")?;
            }
            IrStmt::Ret => {
                writeln!(w, "  lw a0, 0(sp)")?;
                writeln!(w, "  add sp, sp, 4")?;
                writeln!(w, "  j {}_epilogue", f.name)?;
            }
            IrStmt::FrameAddr(a) => {
                writeln!(w, "  addi t0, fp, {}", -12 - (*a as i32) * 4)?;
                writeln!(w, "  sw t0, -4(sp)")?;
                writeln!(w, "  addi sp, sp, -4")?;
            }
            IrStmt::Load => {
                writeln!(w, "  lw t0, 0(sp)")?;
                writeln!(w, "  lw t1, 0(t0)")?;
                writeln!(w, "  sw t1, 0(sp)")?;
            }
            IrStmt::Store => {
                writeln!(w, "  lw t0, 0(sp)")?;
                writeln!(w, "  lw t1, 4(sp)")?;
                writeln!(w, "  sw t1, 0(t0)")?;
                writeln!(w, "  addi sp, sp, 4")?;
            }
            IrStmt::Pop => {
                writeln!(w, "  addi sp, sp, 4")?;
            }
        }
    }
    writeln!(w, "  push 0")?;
    writeln!(w, "{}_epilogue:", f.name)?;
    writeln!(w, "  lw a0, 0(sp)")?;
    writeln!(w, "  addi sp, sp, 4")?;
    writeln!(w, "  lw fp, {}(sp)", frame_size - 8)?;
    writeln!(w, "  lw ra, {}(sp)", frame_size - 4)?;
    writeln!(w, "  addi sp, sp, {}", frame_size)?;
    writeln!(w, "  jr ra")?;
    Ok(())
}

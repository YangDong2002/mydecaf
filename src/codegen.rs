use std::io::{Result, Write};

use crate::{ast::BinaryOp::*, ast::UnaryOp::*, ir::*};

pub fn write_asm(p: &IrProg, w: &mut impl Write) -> Result<()> {
    for (name, (val, siz)) in &p.globals {
        writeln!(w, "    .data")?;
        writeln!(w, "    .globl {}", name)?;
        writeln!(w, "    .align 4")?;
        if siz > 1 {
            assert_eq!(val, 0);
            writeln!(w, "{}:", name)?;
            writeln!(w, "    .zero {}", val)?;
        } else {
            writeln!(w, "    .size {}, {}", name, siz * 4)?;
            writeln!(w, "{}:", name)?;
            writeln!(w, "    .word {}\n", val)?;
        }
    }
    writeln!(w, "    .text")?;
    for f in &p.funcs {
        write_func(&f, w)?;
    }
    Ok(())
}

pub fn write_func(f: &IrFunc, w: &mut impl Write) -> Result<()> {
    eprintln!("{}: ({} locals)", f.name, f.locals);
    for s in &f.stmts {
        eprintln!("  {:?}", s);
    }
    writeln!(w, ".global {}", f.name)?;
    writeln!(w, "{}:", f.name)?;

    let frame_size = (f.locals * 4 + 8) as i32;
    writeln!(w, "  addi sp, sp, {}", -frame_size)?;
    writeln!(w, "  sw ra, {}(sp)", frame_size - 4)?;
    writeln!(w, "  sw fp, {}(sp)", frame_size - 8)?;
    writeln!(w, "  addi fp, sp, {}", frame_size)?;

    for s in &f.stmts {
        writeln!(w, "  # {:?}", s)?;
        match s {
            IrStmt::Const(x) => {
                writeln!(w, "  li t0, {}", x)?;
                writeln!(w, "  addi sp, sp, -4")?;
                writeln!(w, "  sw t0, 0(sp)")?;
            }
            IrStmt::Unary(op) => {
                writeln!(w, "  lw t0, 0(sp)")?;
                writeln!(w, "  {}", match op {
                    UNeg => "neg t0, t0",
                    UNot => "seqz t0, t0",
                    UBNot => "not t0, t0",
                    Deref => "lw t0, 0(t0)",
                    Ref => { unreachable!(); }
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
                writeln!(w, "  j {}_epilogue", f.name)?;
            }
            IrStmt::FrameAddr(a) => {
                writeln!(w, "  addi t0, fp, {}", if *a < 0 { -4 - 4 * *a } else { -12 - a * 4 })?;
                writeln!(w, "  addi sp, sp, -4")?;
                writeln!(w, "  sw t0, 0(sp)")?;
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
            IrStmt::Label(x) => {
                writeln!(w, "label{}:", x)?;
            }
            IrStmt::Beqz(x) => {
                writeln!(w, "  lw t1, 0(sp)")?;
                writeln!(w, "  addi sp, sp, 4")?;
                writeln!(w, "  beqz t1, label{}", x)?;
            }
            IrStmt::Bnez(x) => {
                writeln!(w, "  lw t1, 0(sp)")?;
                writeln!(w, "  addi sp, sp, 4")?;
                writeln!(w, "  bnez t1, label{}", x)?;
            }
            IrStmt::Br(x) => {
                writeln!(w, "  j label{}", x)?;
            }
            IrStmt::Call(name, num) => {
                writeln!(w, "  call {}", name)?;
                writeln!(w, "  addi sp, sp, {}", 4 * ((*num as i32) - 1))?;
                writeln!(w, "  sw a0, 0(sp)")?;
            }
            IrStmt::GlobalAddr(name) => {
                writeln!(w, "  addi sp, sp, -4")?;
                writeln!(w, "  la t1, {}", name)?;
                writeln!(w, "  sw t1, 0(sp)")?;
            }
        }
    }
    writeln!(w, "  addi sp, sp, -4")?;
    writeln!(w, "  sw x0, 0(sp)")?;
    writeln!(w, "{}_epilogue:", f.name)?;
    writeln!(w, "  lw a0, 0(sp)")?;
    writeln!(w, "  addi sp, sp, 4")?;
    writeln!(w, "  lw fp, {}(sp)", frame_size - 8)?;
    writeln!(w, "  lw ra, {}(sp)", frame_size - 4)?;
    writeln!(w, "  addi sp, sp, {}", frame_size)?;
    writeln!(w, "  jr ra")?;
    Ok(())
}

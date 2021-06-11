.global main
main:
  addi sp, sp, -16
  sw ra, 12(sp)
  sw fp, 8(sp)
  addi fp, fp, 16
  # Const(0)
  li t0, 0
  sw t0, -4(sp)
  addi sp, sp, -4
  # FrameAddr(0)
  addi t0, fp, -12
  sw t0, -4(sp)
  addi sp, sp, -4
  # Store
  lw t0, 0(sp)
  lw t1, 4(sp)
  sw t1, 0(t0)
  addi sp, sp, 4
  # Pop
  addi sp, sp, 4
  # Const(0)
  li t0, 0
  sw t0, -4(sp)
  addi sp, sp, -4
  # FrameAddr(1)
  addi t0, fp, -16
  sw t0, -4(sp)
  addi sp, sp, -4
  # Store
  lw t0, 0(sp)
  lw t1, 4(sp)
  sw t1, 0(t0)
  addi sp, sp, 4
  # Pop
  addi sp, sp, 4
  # FrameAddr(1)
  addi t0, fp, -16
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Const(10)
  li t0, 10
  sw t0, -4(sp)
  addi sp, sp, -4
  # Binary(Lt)
  lw t1, 4(sp)
  lw t2, 0(sp)
  slt t1, t1, t2
  addi sp, sp, 4
  sw t1, 0(sp)
  # Beqz(2)
  lw t1, 0(sp)
  addi sp, sp, 4
  beqz t1, label2
  # Label(1)
label1:
  # FrameAddr(0)
  addi t0, fp, -12
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Const(2)
  li t0, 2
  sw t0, -4(sp)
  addi sp, sp, -4
  # Binary(Div)
  lw t1, 4(sp)
  lw t2, 0(sp)
  div t1, t1, t2
  addi sp, sp, 4
  sw t1, 0(sp)
  # Const(2)
  li t0, 2
  sw t0, -4(sp)
  addi sp, sp, -4
  # Binary(Mul)
  lw t1, 4(sp)
  lw t2, 0(sp)
  mul t1, t1, t2
  addi sp, sp, 4
  sw t1, 0(sp)
  # FrameAddr(0)
  addi t0, fp, -12
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Binary(Neq)
  lw t1, 4(sp)
  lw t2, 0(sp)
  xor t1, t1, t2
  snez t1, t1
  addi sp, sp, 4
  sw t1, 0(sp)
  # Beqz(3)
  lw t1, 0(sp)
  addi sp, sp, 4
  beqz t1, label3
  # Br(0)
  j label0
  # Label(3)
label3:
  # FrameAddr(0)
  addi t0, fp, -12
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # FrameAddr(1)
  addi t0, fp, -16
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Binary(Add)
  lw t1, 4(sp)
  lw t2, 0(sp)
  add t1, t1, t2
  addi sp, sp, 4
  sw t1, 0(sp)
  # FrameAddr(0)
  addi t0, fp, -12
  sw t0, -4(sp)
  addi sp, sp, -4
  # Store
  lw t0, 0(sp)
  lw t1, 4(sp)
  sw t1, 0(t0)
  addi sp, sp, 4
  # Pop
  addi sp, sp, 4
  # Label(0)
label0:
  # FrameAddr(1)
  addi t0, fp, -16
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Const(1)
  li t0, 1
  sw t0, -4(sp)
  addi sp, sp, -4
  # Binary(Add)
  lw t1, 4(sp)
  lw t2, 0(sp)
  add t1, t1, t2
  addi sp, sp, 4
  sw t1, 0(sp)
  # FrameAddr(1)
  addi t0, fp, -16
  sw t0, -4(sp)
  addi sp, sp, -4
  # Store
  lw t0, 0(sp)
  lw t1, 4(sp)
  sw t1, 0(t0)
  addi sp, sp, 4
  # FrameAddr(1)
  addi t0, fp, -16
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Const(10)
  li t0, 10
  sw t0, -4(sp)
  addi sp, sp, -4
  # Binary(Lt)
  lw t1, 4(sp)
  lw t2, 0(sp)
  slt t1, t1, t2
  addi sp, sp, 4
  sw t1, 0(sp)
  # Bnez(1)
  lw t1, 0(sp)
  addi sp, sp, 4
  bnez t1, label1
  # Label(2)
label2:
  # FrameAddr(0)
  addi t0, fp, -12
  sw t0, -4(sp)
  addi sp, sp, -4
  # Load
  lw t0, 0(sp)
  lw t1, 0(t0)
  sw t1, 0(sp)
  # Ret
  j main_epilogue
  sw x0, -4(sp)
  addi sp, sp, -4
main_epilogue:
  lw a0, 0(sp)
  addi sp, sp, 4
  lw fp, 8(sp)
  lw ra, 12(sp)
  addi sp, sp, 16
  jr ra

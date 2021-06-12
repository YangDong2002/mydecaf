main:
addi sp, sp,
-12
sw ra,
8(sp)
sw fp,
4(sp)
addi fp, sp,
12
# Const(4)
li t0,
4
sw t0,
-4(sp)
addi sp, sp,
-4
# Const(1)
li t0,
1
sw t0,
-4(sp)
addi sp, sp,
-4
# Const(2)
li t0,
2
sw t0,
-4(sp)
addi sp, sp,
-4
# Binary(Add)
lw t1,
4(sp)
lw t2,
0(sp)
add t1, t1, t2
addi sp, sp,
4
sw t1,
0(sp)
# Call("add", 2)
call add
addi sp, sp,
4
sw a0,
0(sp)
# FrameAddr(0)
addi t0, fp,
-12
sw t0,
-4(sp)
addi sp, sp,
-4
# Store
lw t0,
0(sp)
lw t1,
4(sp)
sw t1,
0(t0)
addi sp, sp,
4
# Pop
addi sp, sp,
4
# FrameAddr(0)
addi t0, fp,
-12
sw t0,
-4(sp)
addi sp, sp,
-4
# Load
lw t0,
0(sp)
lw t1,
0(t0)
sw t1,
0(sp)
# FrameAddr(0)
addi t0, fp,
-12
sw t0,
-4(sp)
addi sp, sp,
-4
# Load
lw t0,
0(sp)
lw t1,
0(t0)
sw t1,
0(sp)
# Binary(Add)
lw t1,
4(sp)
lw t2,
0(sp)
add t1, t1, t2
addi sp, sp,
4
sw t1,
0(sp)
# Ret
j main_epilogue
sw x0,
-4(sp)
addi sp, sp,
-4
main_epilogue:
lw a0,
0(sp)
addi sp, sp,
4
lw fp,
4(sp)
lw ra,
8(sp)
addi sp, sp,
12
jr ra

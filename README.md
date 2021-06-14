# MyDecaf: A Simple MiniDecaf Compiler

`MyDecaf` is a compiler written in Rust. It can translate a programming language similar to C, called `minidecaf`, into
RISCV assembly (runnable in QEMU).

This follows the [tutorial](https://decaf-lang.github.io/minidecaf-tutorial/) from Tsinghua University 2020 Compiler
course. The parser (`src/mparser.rs`) is generated from https://github.com/MashPlant/lalr1. The compiler passed all
the [testcases](https://github.com/decaf-lang/minidecaf-tests) in the course material.

## Features

- Unary, binary arithmetic and logical expressions
- Local & Global variables
- Scope, block statements
- Assignment, `if-else`, `for`, `while`, `do-while`, `break`, `continue`
- Functions, function calls
- Arrays, pointers, arrays of pointers (but no pointer to arrays)
- Lvalue & rvalue inference
- Type checking

For example, it supports the following quicksort testing code:

```c
int qsort(int *a, int l, int r) {
    int i = l;
    int j = r;
    int p = a[(l+r)/2];
    int flag = 1;
    while (i <= j) {
        while (a[i] < p) i = i + 1;
        while (a[j] > p) j = j - 1;
        if (i > j) break;
        int u = a[i]; a[i] = a[j]; a[j] = u;
        i = i + 1;
        j = j - 1;
    }
    if (i < r) qsort(a, i, r);
    if (j > l) qsort(a, l, j);
}

int rand(int *state) {
    *state = *state * 5000087 + 198250529;
    return *state % 1000;
}

int initArr(int n, int *a) {
    int state = 474230941;
    int i = 0;
    while (i < n) {
        a[i] = rand(&state);
        i = i + 1;
    }
}

int isSorted(int n, int *a) {
    int i = 0;
    while (i < n-1) {
        if ( *(a+i) > *(a+i+1) )
            return 0;
        i = i + 1;
    }
    return 1;
}

int n = 1000000;
int a[1000000];

int main() {
    int* ap = (int*) a;
    initArr(n, ap);
    int sorted_before = isSorted(n, ap);
    qsort(ap, 0, n-1);
    int sorted_after = isSorted(n, ap);
    if (!(sorted_before==0 && sorted_after==1))
        return 1;
    return 0;
}
```

## Grammar

```asm
program
    : (function | declaration)*

function
    : type Identifier '(' parameter_list ')' (compound_statement | ';')

type
    : 'int'
    | type '*'

parameter_list
    : (type Identifier (',' type Identifier)*)?

compound_statement
    : '{' block_item* '}'

block_item
    : statement
    | declaration

statement
    : 'return' expression ';'
    | expression? ';'
    | 'if' '(' expression ')' statement ('else' statement)?
    | compound_statement
    | 'for' '(' expression? ';' expression? ';' expression? ')' statement
    | 'for' '(' declaration expression? ';' expression? ')' statement
    | 'while' '(' expression ')' statement
    | 'do' statement 'while' '(' expression ')' ';'
    | 'break' ';'
    | 'continue' ';'

declaration
    : type Identifier ('[' Integer ']')* ('=' expression)? ';'

expression_list
    : (expression (',' expression)*)?

expression
    : assignment

assignment
    : conditional
    | unary '=' expression

conditional
    : logical_or
    | logical_or '?' expression ':' conditional

logical_or
    : logical_and
    | logical_or '||' logical_and

logical_and
    : equality
    | logical_and '&&' equality

equality
    : relational
    | equality ('=='|'!=') relational

relational
    : additive
    | relational ('<'|'>'|'<='|'>=') additive

additive
    : multiplicative
    | additive ('+'|'-') multiplicative

multiplicative
    : unary
    | multiplicative ('*'|'/'|'%') unary

unary
    : postfix
    | ('-'|'~'|'!'|'&'|'*') unary
    | '(' type ')' unary

postfix
    : primary
    | Identifier '(' expression_list ')'
    | postfix '[' expression ']'

primary
    : Integer
    | '(' expression ')'
    | Identifier
```

lvalue rule & type checking rule: See [tutorial](https://decaf-lang.github.io/minidecaf-tutorial/).

## Setting up the environment

As environment configuration is not mentioned in the tutorial, and it is really boring and wasted me a lot of time, I
will briefly summarize the steps below. I work on a Google Cloud server with 8-core CPU, 32 GB Memory, 100 GB SSD,
Ubuntu 20.04.

### Rust

Follow the guide from https://www.rust-lang.org

```bash
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
```

### RISCV GNU Toolchain

Follow the instructions from https://github.com/riscv/riscv-gnu-toolchain . The following sets up the environment (
download source code & compile) on my Google Cloud server in ~1h. **Make sure your internet connection is fast!!!** (
preferably >=10MiB/s)

```bash
git clone https://github.com/riscv/riscv-gnu-toolchain
sudo apt-get install autoconf automake autotools-dev curl python3 libmpc-dev libmpfr-dev libgmp-dev gawk build-essential bison flex texinfo gperf libtool patchutils bc zlib1g-dev libexpat-dev
sudo mkdir -p /opt/risc
cd riscv/riscv-gnu-toolchain/
sudo ./configure --prefix=/opt/riscv --with-arch=rv32im --with-abi=ilp32
sudo make -j8
```

### qemu-riscv32

The following worked for the Google Cloud server:

```bash
sudo apt install qemu-user
```

If you cannot install qemu, you can modify `check.sh` and use `riscv32-unknown-elf-run` compiled previously in RISCV GNU
toolchain instead. **However, it will be extremely slow in step 12, when dealing with large global arrays and lots of
instructions!!!**

## Compilation, Running and debugging

### Compile

```bash
cargo run <source_file> > <assembly_output>
riscv32-unknown-elf-gcc <assembly_output> -o <binary_output>
```

### Run

```bash
qemu-riscv32 <binary_output>
echo $?
```

### Debug

In one terminal:

```shell
qemu-riscv32 <binary_output> -p <port>
```

In another terminal:

```bash
riscv32-unknown-elf-gdb <binary_output>
(gdb) target remote localhost:<port>
```

---

An implementation pitfall in step12: assembly for global array `int a[1048576]`  should be like the following. This
issue together with the inefficient `riscv32-unknown-elf-run` wasted me ~5h more on step 12 and made me bought the
Google cloud server and reconfigured the environment :(

```asm
  .globl a
  .align 2
a:
  .zero 4194304
  .text
```


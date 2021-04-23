pub mod parser;
pub mod ast;
pub mod ir;
pub mod codegen;

pub fn run(input: &str, output: &mut impl std::io::Write) -> std::io::Result<()> {
    let p = parser::Parser{}.parse(&mut parser::Lexer::new(input.as_bytes())).expect("failed to parse input");
    let p = ir::ast2ir(&p);
    codegen::write_asm(&p, output)
}

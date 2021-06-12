pub mod parser;
pub mod ast;
pub mod ir;
pub mod codegen;
pub mod consteval;

pub fn run(input: &str, output: &mut impl std::io::Write) -> std::io::Result<()> {
    let mut p = parser::Lexer::new(input.as_bytes());
    let p = parser::Parser {}.parse(&mut p).expect("failed to parse input");
    eprintln!("Parser output:\n{:#?}", p);
    let p = ir::ast2ir(&p);
    codegen::write_asm(&p, output)
}
use crate::ast::{*, UnaryOp::*};

pub struct Parser {}

impl<'p> Token<'p> {
    fn str(&self) -> &'p str { std::str::from_utf8(self.piece).unwrap() }
    fn parse<T>(&self) -> T where T: std::str::FromStr, <T as std::str::FromStr>::Err: std::fmt::Debug {
        self.str().parse().expect(&format!("(line {}, col {}) ", self.line, self.col))
    }
}

#[parser_macros::lalr1(Prog)]
#[lex=r#"
priority = []

[lexical]
'int' = 'Int'
'return' = 'Return'
';' = 'Semi'
'\(' = 'LPar'
'\)' = 'RPar'
'\{' = 'LBrc'
'\}' = 'RBrc'
'-' = 'UNeg'
'!' = 'UNot'
'~' = 'UBNot'
'\s+' = '_Eps'
'\d+' = 'IntConst'
'[a-zA-Z_]\w*' = 'Id'
"#]

impl<'p> Parser {
    #[rule = "Prog -> Func"]
    fn prog(func: Func<'p>) -> Prog<'p> { Prog { func } }
    #[rule = "Func -> Int Id LPar RPar LBrc Stmt RBrc"]
    fn func(_i: Token, name: Token, _lp: Token, _rp: Token, _lb: Token, stmt: Stmt<'p>, _rb: Token) -> Func<'p> {
        Func { name: name.str(), stmt }
    }
    #[rule = "Stmt -> Return Expr Semi"]
    fn stmt_ret(_r: Token, e: Expr<'p>, _s: Token) -> Stmt<'p> { Stmt::Ret(e) }
    #[rule = "Expr -> IntConst"]
    fn expr_int(i: Token) -> Expr<'p> { Expr::Int(i.parse(), std::marker::PhantomData) }
    #[rule = "Expr -> UNeg Expr"]
    fn expr_uadd(_a: Token, e: Expr<'p>) -> Expr<'p> { Expr::Unary(UNeg, Box::new(e)) }
    #[rule = "Expr -> UNot Expr"]
    fn expr_uadd(_a: Token, e: Expr<'p>) -> Expr<'p> { Expr::Unary(UNot, Box::new(e)) }
    #[rule = "Expr -> UBNot Expr"]
    fn expr_uadd(_a: Token, e: Expr<'p>) -> Expr<'p> { Expr::Unary(UBNot, Box::new(e)) }
}

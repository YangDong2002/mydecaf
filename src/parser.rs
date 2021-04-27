use crate::ast::*;

pub struct Parser {}

impl<'p> Token<'p> {
    fn str(&self) -> &'p str { std::str::from_utf8(self.piece).unwrap() }
    fn parse<T>(&self) -> T where T: std::str::FromStr, <T as std::str::FromStr>::Err: std::fmt::Debug {
        self.str().parse().expect(&format!("(line {}, col {}) ", self.line, self.col))
    }
}

#[parser_macros::lalr1(Prog)]
#[lex=r#"
priority = [
    { assoc = 'left', terms = ['Add', 'Sub'] },
    { assoc = 'left', terms = ['Mul', 'Div', 'Mod'] },
    { assoc = 'no_assoc', terms = ['UNeg', 'UNot', 'UBNot'] }
]

[lexical]
'int' = 'Int'
'return' = 'Return'
';' = 'Semi'
'\(' = 'LPar'
'\)' = 'RPar'
'\{' = 'LBrc'
'\}' = 'RBrc'
'==' = 'Eq'
'!=' = 'Neq'
'>=' = 'Geq'
'>' = 'Gt'
'<=' = 'Leq'
'<' = 'Lt'
'\|\|' = 'LOr'
'&&' = 'LAnd'
'\+' = 'Add'
'-' = 'Sub'
'\*' = 'Mul'
'/' = 'Div'
'%' = 'Mod'
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
    #[rule = "Expr -> LogicalOr"]
    fn expr_lor(o: LogicalOr<'p>) -> Expr<'p> { Expr::LOr(o) }
	#[rule = "LogicalOr -> LogicalAnd"]
	fn lor_land(a: LogicalAnd<'p>) -> LogicalOr<'p> { LogicalOr::LAnd(a) }
	#[rule = "LogicalOr -> LogicalOr LOr LogicalAnd"]
	fn lor_bop(a: LogicalOr<'p>, _: Token, b: LogicalAnd<'p>) -> LogicalOr<'p> { LogicalOr::Bop(Box::new(a), BinaryOp::LOr, b) }
	#[rule = "LogicalAnd -> Equality"]
	fn land_eqn(a: Equality<'p>) -> LogicalAnd<'p> { LogicalAnd::Eqn(a) }
	#[rule = "LogicalAnd -> LogicalAnd LAnd Equality"]
	fn land_bop(a: LogicalAnd<'p>, _: Token, b: Equality<'p>) -> LogicalAnd<'p> { LogicalAnd::Bop(Box::new(a), BinaryOp::LAnd, b) }
	#[rule = "Equality -> Relational"]
	fn eqn_rel(a: Relational<'p>) -> Equality<'p> { Equality::Rel(a) }
	#[rule = "Equality -> Equality Eq Relational"]
	fn eqn_beq(a: Equality<'p>, _: Token, b: Relational<'p>) -> Equality<'p> { Equality::Bop(Box::new(a), BinaryOp::Eqt, b) }
	#[rule = "Equality -> Equality Neq Relational"]
	fn eqn_bneq(a: Equality<'p>, _: Token, b: Relational<'p>) -> Equality<'p> { Equality::Bop(Box::new(a), BinaryOp::Neq, b) }
	#[rule = "Relational -> Additive"]
	fn rel_add(a: Additive<'p>) -> Relational<'p> { Relational::Add(a) }
	#[rule = "Relational -> Relational Geq Additive"]
	fn rel_bgeq(a: Relational<'p>, _: Token, b: Additive<'p>) -> Relational<'p> { Relational::Bop(Box::new(a), BinaryOp::Geq, b) }
	#[rule = "Relational -> Relational Leq Additive"]
	fn rel_bleq(a: Relational<'p>, _: Token, b: Additive<'p>) -> Relational<'p> { Relational::Bop(Box::new(a), BinaryOp::Leq, b) }
	#[rule = "Relational -> Relational Gt Additive"]
	fn rel_bgt(a: Relational<'p>, _: Token, b: Additive<'p>) -> Relational<'p> { Relational::Bop(Box::new(a), BinaryOp::Gt, b) }
	#[rule = "Relational -> Relational Lt Additive"]
	fn rel_blt(a: Relational<'p>, _: Token, b: Additive<'p>) -> Relational<'p> { Relational::Bop(Box::new(a), BinaryOp::Lt, b) }
    #[rule = "Additive -> Multiplicative"]
    fn addi_mul(m: Multiplicative<'p>) -> Additive<'p> { Additive::Mul(m) }
    #[rule = "Additive -> Additive Add Multiplicative"]
    fn addi_bop(adt: Additive<'p>, _: Token, mlt: Multiplicative<'p>) -> Additive<'p> {
        Additive::Bop(Box::new(adt), BinaryOp::Add, mlt)
    }
    #[rule = "Additive -> Additive Sub Multiplicative"]
    fn addi_bop(adt: Additive<'p>, _: Token, mlt: Multiplicative<'p>) -> Additive<'p> {
        Additive::Bop(Box::new(adt), BinaryOp::Sub, mlt)
    }
    #[rule = "Multiplicative -> Unary"]
    fn mult_u(u: Unary<'p>) -> Multiplicative<'p> { Multiplicative::U(u) }
    #[rule = "Multiplicative -> Multiplicative Mul Unary"]
    fn mult_mul(m: Multiplicative<'p>, _: Token, u: Unary<'p>) -> Multiplicative<'p> {
        Multiplicative::Mul(Box::new(m), BinaryOp::Mul, u)
    }
    #[rule = "Multiplicative -> Multiplicative Div Unary"]
    fn mult_mul(m: Multiplicative<'p>, _: Token, u: Unary<'p>) -> Multiplicative<'p> {
        Multiplicative::Mul(Box::new(m), BinaryOp::Div, u)
    }
    #[rule = "Multiplicative -> Multiplicative Mod Unary"]
    fn mult_mul(m: Multiplicative<'p>, _: Token, u: Unary<'p>) -> Multiplicative<'p> {
        Multiplicative::Mul(Box::new(m), BinaryOp::Mod, u)
    }
    #[rule = "Unary -> Primary"]
    fn unary_p(p: Primary<'p>) -> Unary<'p> { Unary::Prim(p) }
    #[rule = "Unary -> Sub Unary"]
    #[prec = "UNeg"]
    fn unary_neg(_: Token, u: Unary<'p>) -> Unary<'p> {
        Unary::Uop(UnaryOp::UNeg, Box::new(u))
    }
    #[rule = "Unary -> UNot Unary"]
    #[prec = "UNot"]
    fn unary_not(_: Token, u: Unary<'p>) -> Unary<'p> {
        Unary::Uop(UnaryOp::UNot, Box::new(u))
    }
    #[rule = "Unary -> UBNot Unary"]
    #[prec = "UBNot"]
    fn unary_bnot(_: Token, u: Unary<'p>) -> Unary<'p> {
        Unary::Uop(UnaryOp::UBNot, Box::new(u))
    }
    #[rule = "Primary -> IntConst"]
    fn prim_int(i: Token) -> Primary<'p> {
        Primary::Int(i.parse(), std::marker::PhantomData)
    }
    #[rule = "Primary -> LPar Expr RPar"]
    fn prim_par(_: Token, e: Expr<'p>, _: Token) -> Primary<'p> {
        Primary::Braced(Box::new(e))
    }
}

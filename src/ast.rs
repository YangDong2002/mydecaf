#[derive(Debug)]
pub struct Prog<'a> {
    pub func: Func<'a>
}

#[derive(Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub stmt: Stmt<'a>
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Ret(Expr<'a>)
}

#[derive(Debug)]
pub enum Expr<'a> {
    Add(Additive<'a>)
}

#[derive(Debug)]
pub enum Additive<'a> {
    Mul(Multiplicative<'a>),
    Bop(Box< Additive<'a> >, BinaryOp, Multiplicative<'a>)
}

#[derive(Debug)]
pub enum Multiplicative<'a> {
    U(Unary<'a>),
    Mul(Box< Multiplicative<'a> >, BinaryOp, Unary<'a>)
}

#[derive(Debug)]
pub enum Unary<'a> {
    Prim(Primary<'a>),
    Uop(UnaryOp, Box< Unary<'a> >)
}

#[derive(Debug)]
pub enum Primary<'a> {
	Int(i32, std::marker::PhantomData<&'a ()>),
    Braced(Box< Expr<'a> >)
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
	UNeg, UNot, UBNot
}
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add, Sub, Mul, Div, Mod
}

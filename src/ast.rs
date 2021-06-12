#[derive(Debug)]
pub struct Prog<'a> {
    pub funcs: Vec<Func<'a>>,
}

#[derive(Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub params: Vec<Declaration<'a>>,
    pub stmts: Option<Vec<BlockItem<'a>>>,
}

#[derive(Debug)]
pub enum BlockItem<'a> {
    Stmt(Stmt<'a>),
    Decl(Declaration<'a>),
}

#[derive(Debug)]
pub enum Stmt<'a> {
    Ret(Expr<'a>),
    MaybeExpr(Option<Expr<'a>>),
    If(Expr<'a>, Box<Stmt<'a>>, Option<Box<Stmt<'a>>>),
    Compound(Vec<BlockItem<'a>>),
    For(Option<Expr<'a>>, Box<Stmt<'a>>, Option<Expr<'a>>),
    DoWhile(Box<Stmt<'a>>, Expr<'a>),
    Break,
    Continue,
}

#[derive(Debug)]
pub struct Declaration<'a> {
    pub name: &'a str,
    pub val: Option<Expr<'a>>,
}

#[derive(Debug)]
pub enum Expr<'a> {
    Assign(&'a str, Box<Expr<'a>>),
    Cond(Conditional<'a>),
}

#[derive(Debug)]
pub enum Conditional<'a> {
    LOr(LogicalOr<'a>),
    Cond(LogicalOr<'a>, Box<Expr<'a>>, Box<Conditional<'a>>),
}

#[derive(Debug)]
pub enum LogicalOr<'a> {
    LAnd(LogicalAnd<'a>),
    Bop(Box<LogicalOr<'a>>, BinaryOp, LogicalAnd<'a>),
}

#[derive(Debug)]
pub enum LogicalAnd<'a> {
    Eqn(Equality<'a>),
    Bop(Box<LogicalAnd<'a>>, BinaryOp, Equality<'a>),
}

#[derive(Debug)]
pub enum Equality<'a> {
    Rel(Relational<'a>),
    Bop(Box<Equality<'a>>, BinaryOp, Relational<'a>),
}

#[derive(Debug)]
pub enum Relational<'a> {
    Add(Additive<'a>),
    Bop(Box<Relational<'a>>, BinaryOp, Additive<'a>),
}

#[derive(Debug)]
pub enum Additive<'a> {
    Mul(Multiplicative<'a>),
    Bop(Box<Additive<'a>>, BinaryOp, Multiplicative<'a>),
}

#[derive(Debug)]
pub enum Multiplicative<'a> {
    U(Unary<'a>),
    Mul(Box<Multiplicative<'a>>, BinaryOp, Unary<'a>),
}

#[derive(Debug)]
pub enum Unary<'a> {
    Prim(Primary<'a>),
    Call(&'a str, Vec<Expr<'a>>),
    Uop(UnaryOp, Box<Unary<'a>>),
}

#[derive(Debug)]
pub enum Primary<'a> {
    Int(i32, std::marker::PhantomData<&'a ()>),
    Braced(Box<Expr<'a>>),
    Identifier(&'a str),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum UnaryOp {
    UNeg,
    UNot,
    UBNot,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Mod,
    LOr,
    LAnd,
    Geq,
    Gt,
    Leq,
    Lt,
    Eqt,
    Neq,
}

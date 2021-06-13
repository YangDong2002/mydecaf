#[derive(Debug)]
pub struct Prog<'a> {
    pub contents: Vec<FuncDecl<'a>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Type {
    pub cnt: usize,
    pub dim: Vec<usize>,
}

pub const SCALAR: Type = Type { cnt: 0, dim: vec![] };

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let vs: Vec<String> = self.dim.iter().map(|x| format!("[{}]", x)).collect();
        write!(f, "'int{}{}'", "*".repeat(self.cnt), vs.join(""))
    }
}


#[derive(Debug)]
pub enum FuncDecl<'a> {
    Func(Func<'a>),
    Decl(Declaration<'a>),
}

#[derive(Debug)]
pub struct Func<'a> {
    pub name: &'a str,
    pub params: Vec<Declaration<'a>>,
    pub stmts: Option<Vec<BlockItem<'a>>>,
    pub ret: Type
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
    pub typ: Type,
    pub name: &'a str,
    pub val: Option<Expr<'a>>
}

#[derive(Debug)]
pub enum Expr<'a> {
    Assign(Unary<'a>, Box<Expr<'a>>),
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
    ExplicitConversion(Type, Box<Unary<'a>>),
    Index(Box<Unary<'a>>, Box<Expr<'a>>),
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
    Deref,
    Ref,
}

pub fn unary_operation(op: UnaryOp, x: (Type, bool)) -> (Type, bool) {
    if x.0.dim.len() > 0 { panic!("No matching unary operator {:?}! Operand is {}", op, x.0) }
    match op {
        UnaryOp::Ref => {
            unreachable!();
        }
        UnaryOp::Deref => {
            if x.0.cnt == 0 { panic!("Type {} cannot be dereferenced!", x.0) }
            return (Type { cnt: x.0.cnt - 1, dim: vec![] }, true)
        }
        _ => {
            if x.0.cnt == 0 { return (SCALAR, false) }
            panic!("No matching unary operator {:?}! Operand is {}", op, x.0)
        }
    }
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

pub fn binary_operation(op: BinaryOp, x: (Type, bool), y: (Type, bool)) -> (Type, bool) {
    if x.0 == SCALAR && y.0 == SCALAR { return (SCALAR, false); }
    if x.0.dim.len() > 0 || y.0.dim.len() > 0 {
        panic!("No matching binary operator {:?}! Operands are {:?} and {:?}", op, x, y);
    }
    if (op == BinaryOp::Eqt || op == BinaryOp::Neq) && x.0.cnt == y.0.cnt { return (SCALAR, false); }
    panic!("No matching binary operator {:?}! Operands are {:?} and {:?}", op, x, y)
}
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
    Int(i32, std::marker::PhantomData<&'a ()>)
}

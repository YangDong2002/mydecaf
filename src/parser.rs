use crate::ast::*;

pub struct Parser;

impl<'p> Token<'p> {
    fn str(&self) -> &'p str { std::str::from_utf8(self.piece).unwrap() }
    fn parse<T>(&self) -> T where T: std::str::FromStr, <T as std::str::FromStr>::Err: std::fmt::Debug {
        self.str().parse().expect(&format!("(line {}, col {}) ", self.line, self.col))
    }
}

fn add_box<T>(x: Option<T>) -> Option<Box<T>> {
    match x {
        Some(u) => { Some(Box::new(u)) }
        None => { None }
    }
}

#[parser_macros::lalr1(Prog)]
#[expand]
#[lex = r#"
priority = [
    { assoc = 'left', terms = ['Add', 'Sub'] },
    { assoc = 'left', terms = ['Mul', 'Div', 'Mod'] },
    { assoc = 'no_assoc', terms = ['UNeg', 'UNot', 'UBNot'] },
    { assoc = 'left', terms = ['Else'] },
]

[lexical]
'int' = 'Int'
'return' = 'Return'
'if' = 'If'
'else' = 'Else'
'for' = 'For'
'do' = 'Do'
'while' = 'While'
'break' = 'Break'
'continue' = 'Continue'
';' = 'Semi'
',' = 'Comma'
'\(' = 'LPar'
'\)' = 'RPar'
'\{' = 'LBrc'
'\}' = 'RBrc'
'\[' = 'LBkt'
'\]' = 'RBkt'
'==' = 'Eq'
'=' = 'Eqto'
':' = 'Colon'
'\?' = 'Quest'
'!=' = 'Neq'
'>=' = 'Geq'
'>' = 'Gt'
'<=' = 'Leq'
'<' = 'Lt'
'\|\|' = 'LOr'
'&&' = 'LAnd'
'&' = 'And'
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
    #[rule = "Prog -> Prog Declaration"]
    fn prog_decl(mut prog: Prog<'p>, d: Declaration<'p>) -> Prog<'p> {
        Prog { contents: (prog.contents.push(FuncDecl::Decl(d)), prog.contents).1 }
    }
    #[rule = "Prog -> Prog Func"]
    fn prog_func(mut prog: Prog<'p>, f: Func<'p>) -> Prog<'p> {
        Prog { contents: (prog.contents.push(FuncDecl::Func(f)), prog.contents).1 }
    }
    #[rule = "Prog ->"]
    fn prog_empty() -> Prog<'p> { Prog { contents: vec![] } }
    #[rule = "Type -> Int"]
    fn type_int(_: Token) -> Type { SCALAR }
    #[rule = "Type -> Type Mul"]
    fn type_pointer(t: Type, _: Token) -> Type { Type { cnt: t.cnt + 1, dim: t.dim } }
    #[rule = "Func -> Type Id LPar Params RPar Compound"]
    fn func_impl(typ: Type, name: Token, _lp: Token, params: Vec<Declaration<'p>>, _rp: Token, stmts: Vec<BlockItem<'p>>) -> Func<'p> {
        Func { name: name.str(), params, stmts: Some(stmts), ret: typ }
    }
    #[rule = "Func -> Type Id LPar Params RPar Semi"]
    fn func_def(typ: Type, name: Token, _lp: Token, params: Vec<Declaration<'p>>, _rp: Token, _s: Token) -> Func<'p> {
        Func { name: name.str(), params, stmts: None, ret: typ }
    }
    #[rule = "Params -> "]
    fn param_empty() -> Vec<Declaration<'p>> { vec![] }
    #[rule = "Params -> MaybeParams Type Id"]
    fn param_last(mut fir: Vec<Declaration<'p>>, typ: Type, name: Token) -> Vec<Declaration<'p>> {
        (fir.push(Declaration { name: name.str(), val: None, typ }), fir).1
    }
    #[rule = "MaybeParams -> "]
    fn maybeparams_empty() -> Vec<Declaration<'p>> { vec![] }
    #[rule = "MaybeParams -> MaybeParams Type Id Comma"]
    fn maybeparams_last(mut fir: Vec<Declaration<'p>>, typ: Type, name: Token, _comma: Token) -> Vec<Declaration<'p>> {
        (fir.push(Declaration { name: name.str(), val: None, typ }), fir).1
    }
    #[rule = "Compound -> LBrc BlockItems RBrc"]
    fn compound(_l: Token, blk: Vec<BlockItem<'p>>, _r: Token) -> Vec<BlockItem<'p>> { blk }
    #[rule = "BlockItems ->"]
    fn block_empty() -> Vec<BlockItem<'p>> { vec![] }
    #[rule = "BlockItems -> BlockItems BlockItem"]
    fn block_one(mut stmts: Vec<BlockItem<'p>>, r: BlockItem<'p>) -> Vec<BlockItem<'p>> {
        (stmts.push(r), stmts).1
    }
    #[rule = "BlockItem -> Stmt"]
    fn block_stmt(stmt: Stmt<'p>) -> BlockItem<'p> { BlockItem::Stmt(stmt) }
    #[rule = "BlockItem -> Declaration"]
    fn block_decl(x: Declaration<'p>) -> BlockItem<'p> { BlockItem::Decl(x) }
    #[rule = "Stmt -> Break Semi"]
    fn stmt_break(_break: Token, _: Token) -> Stmt<'p> { Stmt::Break }
    #[rule = "Stmt -> Continue Semi"]
    fn stmt_continue(_continue: Token, _: Token) -> Stmt<'p> { Stmt::Continue }
    #[rule = "Stmt -> Compound"]
    fn stmt_compound(blk: Vec<BlockItem<'p>>) -> Stmt<'p> { Stmt::Compound(blk) }
    #[rule = "Stmt -> Return Expr Semi"]
    fn stmt_ret(_r: Token, e: Expr<'p>, _s: Token) -> Stmt<'p> { Stmt::Ret(e) }
    #[rule = "Stmt -> MaybeExpr Semi"]
    fn stmt_maybe_expr(m: Option<Expr<'p>>, _s: Token) -> Stmt<'p> { Stmt::MaybeExpr(m) }
    #[rule = "Stmt -> If LPar Expr RPar Stmt MaybeElse"]
    fn stmt_if_else(_if: Token, _lpar: Token, cond: Expr<'p>, _rpar: Token, if_stmt: Stmt<'p>, maybe_else: Option<Stmt<'p>>) -> Stmt<'p> {
        Stmt::If(cond, Box::new(if_stmt), add_box(maybe_else))
    }
    #[rule = "Stmt -> For LPar Declaration MaybeExpr Semi MaybeExpr RPar Stmt"]
    fn stmt_for_decl(_for: Token, _lpar: Token, decl: Declaration<'p>, cond: Option<Expr<'p>>, _s: Token, update: Option<Expr<'p>>, _rpar: Token, body: Stmt<'p>) -> Stmt<'p> {
        Stmt::Compound(vec![
            BlockItem::Decl(decl),
            BlockItem::Stmt(Stmt::For(cond, Box::new(body), update))
        ])
    }
    #[rule = "Stmt -> For LPar MaybeExpr Semi MaybeExpr Semi MaybeExpr RPar Stmt"]
    fn stmt_for_expr(_for: Token, _lpar: Token, init: Option<Expr<'p>>, _s0: Token, cond: Option<Expr<'p>>, _s: Token, update: Option<Expr<'p>>, _rpar: Token, body: Stmt<'p>) -> Stmt<'p> {
        Stmt::Compound(vec![
            BlockItem::Stmt(Stmt::MaybeExpr(init)),
            BlockItem::Stmt(Stmt::For(cond, Box::new(body), update))
        ])
    }
    #[rule = "Stmt -> Do Stmt While LPar Expr RPar"]
    fn stmt_do_while(_do: Token, body: Stmt<'p>, _while: Token, _lp: Token, cond: Expr<'p>, _rp: Token) -> Stmt<'p> {
        Stmt::DoWhile(Box::new(body), cond)
    }
    #[rule = "Stmt -> While LPar Expr RPar Stmt"]
    fn stmt_while(_while: Token, _lp: Token, cond: Expr<'p>, _rp: Token, body: Stmt<'p>) -> Stmt<'p> {
        Stmt::For(Some(cond), Box::new(body), None)
    }
    #[rule = "MaybeElse ->"]
    #[prec = "UNot"]
    fn no_else() -> Option<Stmt<'p>> { None }
    #[rule = "MaybeElse -> Else Stmt"]
    fn have_else(_else: Token, stmt: Stmt<'p>) -> Option<Stmt<'p>> { Some(stmt) }
    #[rule = "MaybeExpr ->"]
    fn maybe_expr_empty() -> Option<Expr<'p>> { None }
    #[rule = "MaybeExpr -> Expr"]
    fn maybe_expr_full(x: Expr<'p>) -> Option<Expr<'p>> { Some(x) }
    #[rule = "Declaration -> Type Id Dim Semi"]
    fn declaration_uninitialized(typ: Type, iden: Token, dim: Vec<usize>, _s: Token) -> Declaration<'p> {
        Declaration { name: iden.str(), val: None, typ: Type { cnt: typ.cnt, dim } }
    }
    #[rule = "Declaration -> Type Id Eqto Expr Semi"]
    fn declaration_initialized(typ: Type, iden: Token, _e: Token, e: Expr<'p>, _s: Token) -> Declaration<'p> {
        Declaration { name: iden.str(), val: Some(e), typ: Type { cnt: typ.cnt, dim: vec![] } }
    }
    #[rule = "Dim ->"]
    fn dim_empty() -> Vec<usize> { vec![] }
    #[rule = "Dim -> Dim LBkt IntConst RBkt"]
    fn dim_last(mut d: Vec<usize>, _lb: Token, x: Token, _rb: Token) -> Vec<usize> {
        (d.push(x.parse()), d).1
    }
    #[rule = "Expr -> Cond"]
    fn expr_cond(cond: Conditional<'p>) -> Expr<'p> { Expr::Cond(cond) }
    #[rule = "Cond -> LogicalOr"]
    fn cond_lor(o: LogicalOr<'p>) -> Conditional<'p> { Conditional::LOr(o) }
    #[rule = "Cond -> LogicalOr Quest Expr Colon Cond"]
    fn cond_cond(o: LogicalOr<'p>, _q: Token, true_exp: Expr<'p>, _c: Token, false_cond: Conditional<'p>) -> Conditional<'p> {
        Conditional::Cond(o, Box::new(true_exp), Box::new(false_cond))
    }
    #[rule = "Expr -> Unary Eqto Expr"]
    fn expr_assign(u: Unary<'p>, _b: Token, c: Expr<'p>) -> Expr<'p> {
        Expr::Assign(u, Box::new(c))
    }
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
    fn addi_add(adt: Additive<'p>, _: Token, mlt: Multiplicative<'p>) -> Additive<'p> {
        Additive::Bop(Box::new(adt), BinaryOp::Add, mlt)
    }
    #[rule = "Additive -> Additive Sub Multiplicative"]
    fn addi_sub(adt: Additive<'p>, _: Token, mlt: Multiplicative<'p>) -> Additive<'p> {
        Additive::Bop(Box::new(adt), BinaryOp::Sub, mlt)
    }
    #[rule = "Multiplicative -> Unary"]
    fn mult_u(u: Unary<'p>) -> Multiplicative<'p> { Multiplicative::U(u) }
    #[rule = "Multiplicative -> Multiplicative Mul Unary"]
    fn mult_mul(m: Multiplicative<'p>, _: Token, u: Unary<'p>) -> Multiplicative<'p> {
        Multiplicative::Mul(Box::new(m), BinaryOp::Mul, u)
    }
    #[rule = "Multiplicative -> Multiplicative Div Unary"]
    fn mult_div(m: Multiplicative<'p>, _: Token, u: Unary<'p>) -> Multiplicative<'p> {
        Multiplicative::Mul(Box::new(m), BinaryOp::Div, u)
    }
    #[rule = "Multiplicative -> Multiplicative Mod Unary"]
    fn mult_mod(m: Multiplicative<'p>, _: Token, u: Unary<'p>) -> Multiplicative<'p> {
        Multiplicative::Mul(Box::new(m), BinaryOp::Mod, u)
    }
    #[rule = "Unary -> Postfix"]
    fn unary_postfix(p: Unary<'p>) -> Unary<'p> { p }
    #[rule = "Postfix -> Primary"]
    fn postfix_primary(p: Primary<'p>) -> Unary<'p> { Unary::Prim(p) }
    #[rule = "Postfix -> Postfix LBkt Expr RBkt"]
    fn postfix_index(p: Unary<'p>, _lb: Token, e: Expr<'p>, _rb: Token) -> Unary<'p> {
        Unary::Index(Box::new(p), Box::new(e))
    }
    #[rule = "Postfix -> Id LPar ExprList RPar"]
    fn postfix_call(name: Token, _lp: Token, exprlist: Vec<Expr<'p>>, _rp: Token) -> Unary<'p> {
        Unary::Call(name.str(), exprlist)
    }
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
    #[rule = "Unary -> Mul Unary"]
    fn unary_deref(_: Token, u: Unary<'p>) -> Unary<'p> { Unary::Uop(UnaryOp::Deref, Box::new(u)) }
    #[rule = "Unary -> And Unary"]
    fn unary_ref(_: Token, u: Unary<'p>) -> Unary<'p> { Unary::Uop(UnaryOp::Ref, Box::new(u)) }
    #[rule = "Unary -> LPar Type RPar Unary"]
    #[prec = "Else"]
    fn unary_conversion(_lp: Token, typ: Type, _rp: Token, u: Unary<'p>) -> Unary<'p> { Unary::ExplicitConversion(typ, Box::new(u)) }
    #[rule = "Primary -> IntConst"]
    fn prim_int(i: Token) -> Primary<'p> {
        Primary::Int(i.parse(), std::marker::PhantomData)
    }
    #[rule = "Primary -> LPar Expr RPar"]
    fn prim_par(_: Token, e: Expr<'p>, _: Token) -> Primary<'p> {
        Primary::Braced(Box::new(e))
    }
    #[rule = "Primary -> Id"]
    fn prim_id(x: Token) -> Primary<'p> {
        Primary::Identifier(x.str())
    }
    #[rule = "ExprList -> "]
    fn exprlist_empty() -> Vec<Expr<'p>> { vec![] }
    #[rule = "ExprList -> MaybeExprList Expr"]
    fn exprlist_last(mut l: Vec<Expr<'p>>, e: Expr<'p>) -> Vec<Expr<'p>> { (l.push(e), l).1 }
    #[rule = "MaybeExprList ->"]
    fn maybeexprlist_empty() -> Vec<Expr<'p>> { vec![] }
    #[rule = "MaybeExprList -> MaybeExprList Expr Comma"]
    fn maybeexprlist_last(mut l: Vec<Expr<'p>>, e: Expr<'p>, _com: Token) -> Vec<Expr<'p>> {
        (l.push(e), l).1
    }
}

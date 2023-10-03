
#[derive(PartialEq, Clone, Debug)]
pub struct Ident(pub String);

#[derive(PartialEq, Clone, Debug)]
pub enum Literal {
    Int(i64),
    String(String),
    Bool(bool),
}

#[derive(PartialEq, Clone, Debug)]
pub enum Expr {
    Literal(Literal),
    Ident(Ident),
    While {
        cond: Box<Expr>,
        consequence: BlockStmt,
    },
    If {
        cond: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    },
}

#[derive(PartialEq, Clone, Debug)]
pub enum Stmt {
    Let(Ident, Expr),
    Break,
    Blank,
    Continue,
    Return(Expr),
    Expr(Expr),
    ReAssign(Ident, Expr)
}

pub type BlockStmt = Vec<Stmt>;

pub type Program = BlockStmt;
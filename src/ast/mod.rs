
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
    Ident(Ident)
}

pub enum Stmt {
    Let(Ident, Expr),
    Return(Expr),
    Expr(Expr),
    ReAssign(Ident, Expr)
}

pub type BlockStmt = Vec<Stmt>;

pub type Program = BlockStmt;
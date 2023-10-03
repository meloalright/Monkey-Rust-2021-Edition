#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    ReturnValue(Box<Object>),
    BreakStatement,
    ContinueStatement,
    Error(String),
    Null,
}
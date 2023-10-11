use std::fmt;
use crate::lexer::unescape::escape_str;

#[derive(PartialEq, Clone, Debug)]
pub enum Object {
    Int(i64),
    String(String),
    Bool(bool),
    ReturnValue(Box<Object>),
    BreakStatement,
    ContinueStatement,
    Error(String),
    Null,
}

/// This is actually repr
impl fmt::Display for Object {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            Object::Int(ref value) => write!(f, "{}", value),
            Object::String(ref value) => write!(f, "{}", escape_str(value)),
            Object::Bool(ref value) => write!(f, "{}", value),
            // Object::Array(ref objects) => {
            //     let mut result = String::new();
            //     for (i, obj) in objects.iter().enumerate() {
            //         if i < 1 {
            //             result.push_str(&format!("{}", obj));
            //         } else {
            //             result.push_str(&format!(", {}", obj));
            //         }
            //     }
            //     write!(f, "[{}]", result)
            // }
            // Object::Hash(ref hash) => {
            //     let mut result = String::new();
            //     for (i, (k, v)) in hash.iter().enumerate() {
            //         if i < 1 {
            //             result.push_str(&format!("{}: {}", k, v));
            //         } else {
            //             result.push_str(&format!(", {}: {}", k, v));
            //         }
            //     }
            //     write!(f, "{{{}}}", result)
            // }
            // Object::Func(ref params, _, _) => {
            //     let mut result = String::new();
            //     for (i, Ident(ref s)) in params.iter().enumerate() {
            //         if i < 1 {
            //             result.push_str(&s.to_string());
            //         } else {
            //             result.push_str(&format!(", {}", s));
            //         }
            //     }
            //     write!(f, "fn({}) {{ ... }}", result)
            // }
            // Object::Builtin(_, _) => write!(f, "[builtin function]"),
            Object::Null => write!(f, "null"),
            Object::BreakStatement => write!(f, "BreakStatement"),
            Object::ContinueStatement => write!(f, "ContinueStatement"),
            Object::ReturnValue(ref value) => write!(f, "ReturnValue({})", value),
            Object::Error(ref value) => write!(f, "Error({})", value),
        }
    }
}
use std::collections::HashMap;
use crate::evaluator::object::Object;

pub fn new_builtins() -> HashMap<String, Object> {
    let mut builtins = HashMap::new();
    builtins.insert(String::from("len"), Object::Builtin(1, monkey_len));
    builtins
}


fn monkey_len(args: Vec<Object>) -> Object {
    match &args[0] {
        Object::String(s) => Object::Int(s.len() as i64),
        Object::Array(o) => Object::Int(o.len() as i64),
        o => Object::Error(format!("argument to `len` not supported, got {}", o)),
    }
}


#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_monkey_len_string() {
        let args = vec![Object::String(String::from("hello"))];
        let expected = Object::Int(5);
        assert_eq!(monkey_len(args), expected);
    }
}
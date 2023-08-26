use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;
use crate::evaluator::object::Object;

#[derive(PartialEq, Clone, Debug)]
pub struct Env {
    pub identifiers: HashMap<String, Object>,
    outer: Option<Rc<RefCell<Env>>>
}

impl Env {
    pub fn new() -> Self {
        Env {
            identifiers: HashMap::new(),
            outer: None
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.identifiers.insert(name, value.clone());
    }

    pub fn get(&mut self, name: String) -> Option<Object> {
        self.identifiers.get(&name).cloned()
    }
}
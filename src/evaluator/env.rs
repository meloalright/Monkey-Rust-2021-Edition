use crate::evaluator::object::Object;
use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(PartialEq, Clone, Debug)]
struct VariableStatus {
    constant: bool,
}

#[derive(PartialEq, Clone, Debug)]
pub struct Env {
    pub identifiers: HashMap<String, Object>,
    variables_status: HashMap<String, VariableStatus>,
    outer: Option<Rc<RefCell<Env>>>,
}

impl Env {
    pub fn new() -> Self {
        Env {
            identifiers: HashMap::new(),
            variables_status: HashMap::new(),
            outer: None,
        }
    }

    pub fn from(builtins: HashMap<String, Object>) -> Self {
        Env {
            identifiers: builtins,
            variables_status: HashMap::new(),
            outer: None,
        }
    }

    pub fn new_with_outer(outer: Rc<RefCell<Env>>) -> Self {
        Env {
            identifiers: HashMap::new(),
            variables_status: HashMap::new(),
            outer: Some(outer),
        }
    }

    pub fn set(&mut self, name: String, value: Object) {
        self.identifiers.insert(name, value.clone());
    }

    pub fn get(&mut self, name: String) -> Option<Object> {
        match self.identifiers.get(&name) {
            Some(value) => Some(value.clone()),
            None => match self.outer {
                Some(ref outer) => outer.borrow_mut().get(name),
                None => None,
            },
        }
    }
}

impl Env {
    pub fn constant(&mut self, name: String) {
        match self.variables_status.get_mut(&name) {
            Some(variable_status) => {
                variable_status.constant = true;
            }
            None => {
                self.variables_status
                    .insert(name, VariableStatus { constant: true });
            }
        }
    }

    pub fn is_constant(&mut self, name: String) -> bool {
        match self.variables_status.get(&name) {
            Some(variable_status) => variable_status.constant == true,
            None => false,
        }
    }
}

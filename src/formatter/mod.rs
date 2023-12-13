use crate::ast::*;

pub struct Formatter {
    indent: usize,
    column: usize,
    max_one_line_chars: usize,
    max_one_line_hash: usize,
}

impl Formatter {

    pub fn default() -> Self {
        Self {
            indent: 0,
            column: 1,
            max_one_line_chars: 80,
            max_one_line_hash: 3,
        }
    }

    pub fn format(&mut self, program: Program) -> String {
        self.format_block_stmt(program)
    }

    pub fn format_block_stmt(&mut self, stmts: BlockStmt) -> String {
        let mut result = String::new();
        // normalize
        let list = stmts;

        for (i, stmt) in list.into_iter().enumerate() {
            self.column = self.indent * 2 + 1;

            if i > 0 {
                result.push_str("\n");
            }

            let indent_str = if stmt == Stmt::Blank {
                String::new()
            } else {
                self.indent_str(0)
            };

            result.push_str(&format!("{}{}", indent_str, self.format_stmt(stmt)));
        }

        result
    }

    fn indent_str(&self, offset: i32) -> String {
        let indent = self.indent as i32;
        let size = indent + offset;
        // let size = if indent >= offset { indent + offset } else { 0 }; // TODO

        "  ".repeat(size as usize)
    }
}

// expr format processor

impl Formatter {

    fn format_stmt(&mut self, stmt: Stmt) -> String {
        match stmt {
            // Stmt::Let(ident, expr) => self.format_let_stmt(ident, expr),
            // Stmt::Return(expr) => self.format_return_stmt(expr),
            Stmt::Expr(expr) => {
                // if Self::ignore_semicolon_expr(&expr) {
                    // self.format_expr(expr, Precedence::Lowest)
                // } else {
                    format!("{};", self.format_expr(expr, Precedence::Lowest))
                // }
            }
            Stmt::Blank => String::new(),
            _ => todo!()
        }
    }

    fn format_expr(&mut self, expr: Expr, precedence: Precedence) -> String {
        match expr {
            Expr::Ident(ident) => self.format_ident_expr(ident),
            Expr::Literal(literal) => self.format_literal(literal),
            Expr::Prefix(prefix, right) => self.format_prefix_expr(prefix, right),
            Expr::Infix(infix, left, right) => {
                self.format_infix_expr(infix, left, right, precedence)
            }
            // Expr::Index(left, index) => self.format_index_expr(left, index),
            Expr::If {
                cond,
                consequence,
                alternative,
            } => self.format_if_expr(cond, consequence, alternative),
            // Expr::Func { params, body } => self.format_func_expr(params, body),
            Expr::Call { func, args } => self.format_call_expr(func, args),
            other => format!("{:?}", other)
        }
    }

    fn format_prefix_expr(&mut self, prefix: Prefix, right: Box<Expr>) -> String {
        let right_str = self.format_expr(*right, Precedence::Prefix);

        format!("{}{}", prefix, right_str)
    }

    fn format_infix_expr(
        &mut self,
        infix: Infix,
        left: Box<Expr>,
        right: Box<Expr>,
        precedence: Precedence,
    ) -> String {
        let current_precedence = Self::infix_to_precedence(&infix);
        let left_str = self.format_expr(*left, current_precedence.clone());
        let right_str = self.format_expr(*right, current_precedence.clone());

        if precedence >= current_precedence {
            format!("({} {} {})", left_str, infix, right_str)
        } else {
            format!("{} {} {}", left_str, infix, right_str)
        }
    }


    fn format_literal(&mut self, literal: Literal) -> String {
        match literal {
            Literal::Int(value) => self.format_int_literal(value),
            Literal::String(value) => self.format_string_literal(value),
            Literal::Bool(value) => self.format_bool_literal(value),
            _ => todo!()
            // Literal::Array(value) => self.format_array_literal(value, false),
            // Literal::Hash(value) => self.format_hash_literal(value, false),
        }
    }


    fn format_int_literal(&mut self, value: i64) -> String {
        let result = value.to_string();
        self.column += result.len();
        result
    }

    fn format_string_literal(&mut self, value: String) -> String {
        let result = format!("\"{}\"", value);
        self.column += result.len();
        result
    }

    fn format_bool_literal(&mut self, value: bool) -> String {
        let result = value.to_string();
        self.column += result.len();
        result
    }

    fn format_ident_expr(&mut self, ident: Ident) -> String {
        let Ident(ident_str) = ident;
        self.column += ident_str.len();
        ident_str
    }
}


// condition format processor
impl Formatter {
    fn format_if_expr(
        &mut self,
        cond: Box<Expr>,
        consequence: BlockStmt,
        alternative: Option<BlockStmt>,
    ) -> String {
        let cond_str = self.format_expr(*cond, Precedence::Lowest);

        self.indent += 1;

        let consequence_str = self.format_block_stmt(consequence);

        let result = match alternative {
            Some(alternative_expr) => {
                let alternative_str = self.format_block_stmt(alternative_expr);
                let indent_str = self.indent_str(-1);
                format!(
                    "if ({}) {{\n{}\n{}}} else {{\n{}\n{}}}",
                    cond_str, consequence_str, indent_str, alternative_str, indent_str,
                )
            }
            None => {
                let indent_str = self.indent_str(-1);
                format!(
                    "if ({}) {{\n{}\n{}}}",
                    cond_str, consequence_str, indent_str
                )
            }
        };

        self.indent -= 1;

        result
    }
}

// function format processor
impl Formatter {
    fn format_call_expr(&mut self, func: Box<Expr>, args: Vec<Expr>) -> String {
        let func_str = self.format_expr(*func, Precedence::Lowest);
        let mut args_str = String::new();

        for (i, arg) in args.into_iter().enumerate() {
            if i > 0 {
                args_str.push_str(", ");
            }

            args_str.push_str(&self.format_expr(arg, Precedence::Lowest));
        }

        format!("{}({})", func_str, args_str)
    }
}

// precedence format processor
impl Formatter {
    fn infix_to_precedence(infix: &Infix) -> Precedence {
        match infix {
            Infix::Plus | Infix::Minus => Precedence::Sum,
            Infix::Multiply | Infix::Divide => Precedence::Product,
            Infix::LT | Infix::LTEQ => Precedence::LessGreater,
            Infix::GT | Infix::GTEQ => Precedence::LessGreater,
            Infix::Equal | Infix::NotEqual => Precedence::Equals,
        }
    }
}
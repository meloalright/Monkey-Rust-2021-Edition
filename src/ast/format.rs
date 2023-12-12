use crate::ast::{ Stmt, Expr, Prefix, Infix, Literal };

impl Stmt {
    pub fn to_string(&self) -> String {
        match self {
            Stmt::Expr(expr) => {
                expr.to_string()
            },
            Stmt::Let(ident_name, expr) => {
                format!("let {:?} = {}", ident_name, expr.to_string())
            },
            _ => todo!()
        }
    }
}

impl Expr {
    pub fn to_string(&self) -> String {
        match self {
            Expr::Infix(infix, left_expr, right_expr) => {
                format!("({} {} {})", left_expr.to_string(), infix.to_string(), right_expr.to_string())
            },
            Expr::Literal(literal) => {
                literal.to_string()
            },
            Expr::If { cond, consequence, alternative } => {
                let mut fmt = format!("if {} {{ ", cond.to_owned().to_string());
                for con in consequence.iter() {
                    fmt.push_str(con.to_string().as_str());
                    fmt.push_str("; ");
                }
                fmt.push_str(" }");
                if let Some(alternative) = alternative {
                    fmt.push_str("else { ");
                    for alt in alternative.iter() {
                        fmt.push_str(alt.to_string().as_str());
                        fmt.push_str("; ");
                    }
                    fmt.push_str(" }");
                }
                fmt
            },
            Expr::Call { func, args } => {
                let mut fmt = "".to_owned();
                fmt.push_str((*func).to_string().as_str());
                fmt.push_str("(");
                for (i, arg) in args.iter().enumerate() {
                    fmt.push_str(arg.to_string().as_str());
                    if i < args.len() - 1 {
                        fmt.push_str(", ");
                    } else {
                        fmt.push_str(")")
                    }
                }
                fmt
            },
            Expr::Ident(ident) => {
                format!("{}", ident.0)
            },
            Expr::Prefix(prefix, expr) => {
                format!("({}{})", prefix.to_string(), (*expr).to_string())
            },
            Expr::Index(object, key) => {
                format!("({}[{}])", (*object).to_string(), (*key).to_string())
            },
            expr => {
                format!("{:?}", expr)
            },
            _ => todo!()
        }
    }
}

impl Prefix {
    pub fn to_string(&self) -> String {
        match self {
            Prefix::Not => String::from("!"),
            Prefix::Minus => String::from("-"),
            _ => todo!()
        }
    }
}

impl Infix {
    pub fn to_string(&self) -> String {
        match self {
            Infix::Plus => format!("{}", "+"),
            Infix::Minus => format!("{}", "-"),
            Infix::GT => format!("{}", ">"),
            Infix::LT => format!("{}", "<"),
            Infix::Multiply => format!("{}", "*"),
            Infix::Divide => format!("{}", "/"),
            Infix::Equal => format!("{}", "=="),
            Infix::NotEqual => format!("{}", "!="),
            _ => todo!()
        }
    }
}

impl Literal {
    pub fn to_string(&self) -> String {
        match self {
            Literal::Int(x) => format!("{}", x),
            Literal::String(str) => format!("\"{}\"", str),
            Literal::Bool(bool) => format!("{}", bool),
            Literal::Array(array) => {
                let mut fmt = "".to_owned();
                fmt.push_str("[");
                for (i, arg) in array.iter().enumerate() {
                    fmt.push_str(arg.to_string().as_str());
                    if i < array.len() - 1 {
                        fmt.push_str(", ");
                    } else {
                        fmt.push_str("]")
                    }
                }
                fmt
            },
            _ => todo!()
        }
    }
}


#[cfg(test)]
mod tests {
    use crate::ast;
    use crate::evaluator::builtins::new_builtins;
    use crate::lexer::Lexer;
    use crate::parser::Parser;
    use std::cell::RefCell;
    use std::rc::Rc;

    // fn eval(input: &str) -> Option<object::Object> {
    //     Evaluator {
    //         env: Rc::new(RefCell::new(env::Env::from(new_builtins()))),
    //     }
    //     .eval(&Parser::new(Lexer::new(input)).parse())
    // }

    fn format(input: &str) -> String {
        let mut program = Parser::new(Lexer::new(input)).parse();
        program[0].to_string()
    }


    #[test]
    fn test_expr_format() {
        let tests = vec![
            ("-a * b", "((-a) * b)"),
            ("!-a", "(!(-a))"),
            ("a + b + c", "((a + b) + c)"),
            (
                "a + b - c",
                "((a + b) - c)",
            ),
            (
                "a * b * c",
                "((a * b) * c)",
            ),
            (
                "a * b / c",
                "((a * b) / c)",
            ),
            (
                "a + b / c",
                "(a + (b / c))",
            ),
            (
                "a + b * c + d / e - f",
                "(((a + (b * c)) + (d / e)) - f)",
            ),
            // TODO
            // (
            //     "3 + 4; -5 * 5",
            //     "(3 + 4)((-5) * 5)",
            // ),
            (
                "5 > 4 == 3 < 4",
                "((5 > 4) == (3 < 4))",
            ),
            (
                "5 < 4 != 3 > 4",
                "((5 < 4) != (3 > 4))",
            ),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            (
                "true",
                "true",
            ),
            (
                "false",
                "false",
            ),
            (
                "3 > 5 == false",
                "((3 > 5) == false)",
            ),
            (
                "3 < 5 == true",
                "((3 < 5) == true)",
            ),
            (
                "a + add(b * c) + d",
                "((a + add((b * c))) + d)",
            ),
            (
                "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
                "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
            ),
            (
                "add(a + b + c * d / f + g)",
                "add((((a + b) + ((c * d) / f)) + g))",
            ),
            (
                "a * [1, 2, 3, 4][b * c] * d",
                "((a * ([1, 2, 3, 4][(b * c)])) * d)",
            ),
            (
                "add(a * b[2], b[1], 2 * [1, 2][1])",
                "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
            ),
        ];

        for (input, expect) in tests {
            assert_eq!(expect, format(input));
        }
    }
}
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
                format!("{}{}", prefix.to_string(), (*expr).to_string())
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
            Prefix::Not => format!("{}", ""),
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
            _ => todo!()
        }
    }
}

impl Literal {
    pub fn to_string(&self) -> String {
        match self {
            Literal::Int(x) => format!("{}", x),
            Literal::String(str) => format!("\"{}\"", str),
            _ => todo!()
        }
    }
}

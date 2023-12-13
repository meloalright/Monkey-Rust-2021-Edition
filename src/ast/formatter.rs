

// #[cfg(test)]
// mod tests {
//     use crate::ast;
//     use crate::evaluator::builtins::new_builtins;
//     use crate::lexer::Lexer;
//     use crate::parser::Parser;
//     use std::cell::RefCell;
//     use std::rc::Rc;

//     // fn eval(input: &str) -> Option<object::Object> {
//     //     Evaluator {
//     //         env: Rc::new(RefCell::new(env::Env::from(new_builtins()))),
//     //     }
//     //     .eval(&Parser::new(Lexer::new(input)).parse())
//     // }

//     fn format(input: &str) -> String {
//         let mut program = Parser::new(Lexer::new(input)).parse();
//         program[0].to_string()
//     }


//     #[test]
//     fn test_let_stmt_format() {
//         let tests = vec![
//             ("let a = 3", "let a = 3;"),
//             (" let a=  3", "let a = 3;"),
//         ];

//         for (input, expect) in tests {
//             assert_eq!(expect, format(input));
//         }
//     }


//     #[test]
//     fn test_literal_format() {
//         let tests = vec![
//             ("1000", "1000;"),
//             ("\"foo\"", "\"foo\";"),
//         ];

//         for (input, expect) in tests {
//             assert_eq!(expect, format(input));
//         }
//     }


//     #[test]
//     fn test_return_format() {
//         let tests = vec![
//             ("return   100", "return 100;"),
//             ("return [100,100]", "return [100, 100];"),
//             ("return [\"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa\"]", r#"return [
//   "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
// ];"#),
//         ];

//         for (input, expect) in tests {
//             assert_eq!(expect, format(input));
//         }
//     }


//     #[test]
//     fn test_expr_format() {
//         let tests = vec![
//             ("-a * b", "((-a) * b)"),
//             ("!-a", "(!(-a))"),
//             ("a + b + c", "((a + b) + c)"),
//             (
//                 "a + b - c",
//                 "((a + b) - c)",
//             ),
//             (
//                 "a * b * c",
//                 "((a * b) * c)",
//             ),
//             (
//                 "a * b / c",
//                 "((a * b) / c)",
//             ),
//             (
//                 "a + b / c",
//                 "(a + (b / c))",
//             ),
//             (
//                 "a + b * c + d / e - f",
//                 "(((a + (b * c)) + (d / e)) - f)",
//             ),
//             // TODO
//             // (
//             //     "3 + 4; -5 * 5",
//             //     "(3 + 4)((-5) * 5)",
//             // ),
//             (
//                 "5 > 4 == 3 < 4",
//                 "((5 > 4) == (3 < 4))",
//             ),
//             (
//                 "5 < 4 != 3 > 4",
//                 "((5 < 4) != (3 > 4))",
//             ),
//             (
//                 "3 + 4 * 5 == 3 * 1 + 4 * 5",
//                 "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
//             ),
//             (
//                 "true",
//                 "true",
//             ),
//             (
//                 "false",
//                 "false",
//             ),
//             (
//                 "3 > 5 == false",
//                 "((3 > 5) == false)",
//             ),
//             (
//                 "3 < 5 == true",
//                 "((3 < 5) == true)",
//             ),
//             (
//                 "a + add(b * c) + d",
//                 "((a + add((b * c))) + d)",
//             ),
//             (
//                 "add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
//                 "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))",
//             ),
//             (
//                 "add(a + b + c * d / f + g)",
//                 "add((((a + b) + ((c * d) / f)) + g))",
//             ),
//             (
//                 "a * [1, 2, 3, 4][b * c] * d",
//                 "((a * ([1, 2, 3, 4][(b * c)])) * d)",
//             ),
//             (
//                 "add(a * b[2], b[1], 2 * [1, 2][1])",
//                 "add((a * (b[2])), (b[1]), (2 * ([1, 2][1])))",
//             ),
//         ];

//         for (input, expect) in tests {
//             assert_eq!(expect, format(input));
//         }
//     }
// }
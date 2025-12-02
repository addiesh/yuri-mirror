use thin_vec::thin_vec;
use yuri_ast::expression::{
    BinaryExpr, BlockExpr, CallExpr, Expression, LiteralExpr, PathExpr, UnaryExpr,
};
use yuri_ast::item::{FunctionItem, OuterDeclaration};
use yuri_ast::types::WrittenTy;
use yuri_ast::{InStorage, Qpath};
use yuri_common::{BinaryOperator, FloatBits, ScalarTy, UnaryOperator};

use crate::Ast;
use crate::{ParseState, parse_all};

macro_rules! parse_expression_test {
    { $test_name:ident, $source:expr, $expect:expr } => {
        #[test]
        fn $test_name() {
            let source = $source;
            let tokens: Box<[_]> = yuri_lexer::tokenize(source).collect();
            let mut storage = InStorage::default();

            let mut state = ParseState {
                storage: &mut storage,
                source,
                tokens: &tokens,
                index: 0,
                byte_offset: 0,
                errors: Vec::new(),
            };

            for tok in &tokens {
                println!(" - {tok:?}");
            }

            let thing = state.expression().unwrap();
            if !state.errors.is_empty() {
                eprintln!("parse errors!");
                for error in state.errors {
                    eprintln!(" - {error}");
                }
                panic!();
            }
            let expected: fn(state: &mut ParseState) -> Expression = $expect;
            assert_eq!(thing, expected(&mut state));
            println!("{thing:?}");
        }
    };
}

macro_rules! parse_test {
    { $test_name:ident, $source:expr, $expect:expr } => {
        #[test]
        fn $test_name() {
            let source = $source;
            let tokens: Box<[_]> = yuri_lexer::tokenize(source).collect();
            let mut storage = InStorage::default();

            for tok in &tokens {
                println!(" - {tok:?}");
            }

            let (ast, errors, mut state) = parse_all(source, &mut storage, &tokens);

            if !errors.is_empty() {
                eprintln!("parse errors!");
                for error in errors {
                    eprintln!(" - {error}");
                }
                panic!();
            }
            let expected: fn(state: &mut ParseState) -> Ast = $expect;
            assert_eq!(ast, expected(&mut state));
            println!("{ast:?}");
        }
    };
}

parse_expression_test! {
    simple_equality,
    "x == y",
    |state| {
        BinaryExpr::new_e(
            BinaryOperator::Eq,
            Expression::Access(state.str_to_ident("x")),
            Expression::Access(state.str_to_ident("y")),
        )
    }
}

parse_expression_test! {
    grouped_simple_equality,
    "(x == y)",
    |state| {
        Expression::Paren(BinaryExpr::new_e(
            BinaryOperator::Eq,
            Expression::Access(state.str_to_ident("x")),
            Expression::Access(state.str_to_ident("y")),
        ).into())
    }
}

parse_expression_test! {
    grouped_simple_equality_equality,
    "(x == y) != z",
    |state| {
        BinaryExpr::new_e(
            BinaryOperator::NotEq,
            Expression::Paren(BinaryExpr::new_e(
                BinaryOperator::Eq,
                Expression::Access(state.str_to_ident("x")),
                Expression::Access(state.str_to_ident("y")),
            ).into()),
            Expression::Access(state.str_to_ident("z")),
        )
    }
}

parse_expression_test! {
    function_call,
    "floor(1.0)",
    |state| {
        CallExpr::new_e(
            Expression::Access(state.str_to_ident("floor")),
            [LiteralExpr::Decimal(1.0)],
        )
    }
}

parse_expression_test! {
    field_access,
    "x.y.z",
    |state| {
        PathExpr::new_e(
            Expression::Access(state.str_to_ident("x")),
            [
                state.str_to_ident("y"),
                state.str_to_ident("z"),
            ],
        )
    }
}

parse_expression_test! {
    field_access_unary,
    "+x.y.z",
    |state| {
        UnaryExpr::new_e(
            UnaryOperator::Positive,
            PathExpr::new_e(
                Expression::Access(state.str_to_ident("x")),
                [
                    state.str_to_ident("y"),
                    state.str_to_ident("z"),
                ],
            )
        )
    }
}

parse_expression_test! {
    method_call,
    "x.y(1.0)",
    |state| {
        CallExpr::new_e(
            PathExpr::new_e(
                Expression::Access(state.str_to_ident("x")),
                [state.str_to_ident("y")],
            ),
            [LiteralExpr::Decimal(1.0)],
        )
    }
}

parse_expression_test! {
    method_call_and_field,
    "x.y(1.0).xyz",
    |state| {
        PathExpr::new_e(
            CallExpr::new_e(
                PathExpr::new_e(
                    Expression::Access(state.str_to_ident("x")),
                    [state.str_to_ident("y")],
                ),
                [LiteralExpr::Decimal(1.0)],
            ),
            [
                state.str_to_ident("xyz"),
            ],
        )
    }
}

parse_expression_test! {
    complicated_expression,
    "1 + 2 * 3 / 4 == 5 or 6 != +~7",
    |_| {
        BinaryExpr::new_e(
            BinaryOperator::LogicOr,
            BinaryExpr::new_e(
                BinaryOperator::Eq,
                BinaryExpr::new_e(
                    BinaryOperator::Add,
                    LiteralExpr::Number(1),
                    BinaryExpr::new_e(
                        BinaryOperator::Multiply,
                        LiteralExpr::Number(2),
                        BinaryExpr::new_e(
                            BinaryOperator::Divide,
                            LiteralExpr::Number(3),
                            LiteralExpr::Number(4),
                        )
                    )
                ),
                LiteralExpr::Number(5)
            ),
            BinaryExpr::new_e(
                BinaryOperator::NotEq,
                LiteralExpr::Number(6),
                UnaryExpr::new_e(
                    UnaryOperator::Positive,
                    UnaryExpr::new_e(
                        UnaryOperator::BitwiseNot,
                        LiteralExpr::Number(7),
                    )
                )
            )
        )
    }
}

parse_test! {
    simple_function,
    "fn main(): f32 {}",
    |state| vec![
        FunctionItem {
            attributes: vec![],
            export: false,
            name: state.str_to_ident("main"),
            parameters: vec![],
            return_type: WrittenTy::Scalar(ScalarTy::Float(FloatBits::Float32)),
            body: BlockExpr { statements: vec![] }
        }.into()
    ]
}

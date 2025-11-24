use thin_vec::thin_vec;

use yuri_common::{BinaryOperator, UnaryOperator};

use crate::expression::{BinaryExpression, Expression, LiteralExpression, UnaryExpression};
use crate::parse::ParseState;
use crate::{ParseStorage, Qpath};

macro_rules! parse_assert {
    { $test_name:ident, $source:expr, $expect:expr } => {
        #[test]
        fn $test_name() {
            let source = $source;
            let tokens: Box<[_]> = yuri_lexer::tokenize(source).collect();
            let mut storage = ParseStorage::default();

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

parse_assert! {
    simple_equality,
    "x == y",
    |state| {
        BinaryExpression::new_e(
            BinaryOperator::Eq,
            Expression::Variable(Qpath(thin_vec![state.str_to_ident("x")])),
            Expression::Variable(Qpath(thin_vec![state.str_to_ident("y")])),
        )
    }
}

parse_assert! {
    complicated_expression,
    "1 + 2 * 3 / 4 == 5 or 6 != +~7",
    |_| {
        BinaryExpression::new_e(
            BinaryOperator::LogicOr,
            BinaryExpression::new_e(
                BinaryOperator::Eq,
                BinaryExpression::new_e(
                    BinaryOperator::Add,
                    LiteralExpression::Number(1),
                    BinaryExpression::new_e(
                        BinaryOperator::Multiply,
                        LiteralExpression::Number(2),
                        BinaryExpression::new_e(
                            BinaryOperator::Divide,
                            LiteralExpression::Number(3),
                            LiteralExpression::Number(4),
                        )
                    )
                ),
                LiteralExpression::Number(5)
            ),
            BinaryExpression::new_e(
                BinaryOperator::NotEq,
                LiteralExpression::Number(6),
                UnaryExpression::new_e(
                    UnaryOperator::Positive,
                    UnaryExpression::new_e(
                        UnaryOperator::BitwiseNot,
                        LiteralExpression::Number(7),
                    )
                )
            )
        )
    }
}

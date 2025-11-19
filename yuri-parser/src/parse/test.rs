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

            let thing = state.expr_compare().unwrap();
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
        BinaryExpression {
            operator: BinaryOperator::Eq,
            left: Expression::Variable(Qpath(thin_vec![state.str_to_ident("x")])).into(),
            right: Expression::Variable(Qpath(thin_vec![state.str_to_ident("y")])).into(),
        }
        .into()
    }
}

parse_assert! {
    complicated_expression,
    "1 + 2 * 3 / 4 == 5 or 6 != +~7",
    |_| {
        BinaryExpression {
            operator: BinaryOperator::LogicOr,
            left: BinaryExpression {
                operator: BinaryOperator::Eq,
                left: BinaryExpression {
                    operator: BinaryOperator::Add,
                    left: LiteralExpression::Number(1).into(),
                    right: BinaryExpression {
                        operator: BinaryOperator::Multiply,
                        left: LiteralExpression::Number(2).into(),
                        right: BinaryExpression {
                            operator: BinaryOperator::Divide,
                            left: LiteralExpression::Number(3).into(),
                            right: LiteralExpression::Number(4).into(),
                        }
                        .into(),
                    }
                    .into(),
                }
                .into(),
                right: LiteralExpression::Number(5).into(),
            }
            .into(),
            right: Expression::Binary(BinaryExpression {
                operator: BinaryOperator::NotEq,
                left: LiteralExpression::Number(6).into(),
                right: UnaryExpression {
                    operator: UnaryOperator::Positive,
                    value: UnaryExpression {
                        operator: UnaryOperator::BitwiseNot,
                        value: LiteralExpression::Number(7).into(),
                    }
                    .into(),
                }
                .into(),
            })
            .into(),
        }
        .into()
    }
}

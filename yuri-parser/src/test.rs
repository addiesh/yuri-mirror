use thin_vec::thin_vec;
use yuri_ast::expression::{
    BinaryExpr, BlockExpr, BlockStatement, CallExpr, CompoundExpr, CompoundExprField, Expression,
    FieldExpr, LiteralExpr, UnaryExpr,
};
use yuri_ast::item::{Attribute, FunctionItem, OuterDeclaration, ParameterItem, TypeAliasItem};
use yuri_ast::pretty::AstPretty;
use yuri_ast::types::{
    CompoundTy, CompoundTyField, MatrixTy, VectorTy, WRITTEN_VEC2F, WRITTEN_VEC3F, WRITTEN_VEC4F,
    WrittenTy,
};
use yuri_ast::{Ident, InStorage, Keyword, Qpath, VectorRepr};
use yuri_common::{BinaryOperator, DimCount, FloatBits, ScalarTy, UnaryOperator};

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

            for (index, tok) in tokens.iter().enumerate() {
                println!(" {index}.\t {tok:?}");
            }

            let (ast, mut state) = parse_all(source, &mut storage, &tokens);

            if !state.errors.is_empty() {
                eprintln!("parse errors!");
                for error in state.errors {
                    eprintln!(" - {error}");
                }
                panic!();
            }
            let expected: fn(state: &mut ParseState) -> Ast = $expect;
            let expected = expected(&mut state);
            assert_eq!(ast, expected);
            // if ast != expected {
            //     let got_map = ast.iter().map(|od| od.pretty_print_ast(&storage, 0)).collect::<Vec<_>>().join(";\n");
            //     let expect_map = expected.iter().map(|od| od.pretty_print_ast(&storage, 0)).collect::<Vec<_>>().join(";\n");
            //     panic!("ASTs are not equal!\n~~~~~~~~~~ Expected: ~~~~~~~~~~\n{}\n\n~~~~~~~~~~ Got: ~~~~~~~~~~\n{}", expect_map, got_map)
            // }
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
    multiple_function_arguments,
    "vec4f(1, 2, 3, 4)",
    |_| {
        CallExpr::new_e(
            Expression::Access(Ident::Keyword(Keyword::Vec(VectorTy { size: DimCount::Four, repr: VectorRepr::Float(None) }))),
            [
                LiteralExpr::Integer(1),
                LiteralExpr::Integer(2),
                LiteralExpr::Integer(3),
                LiteralExpr::Integer(4),
            ],
        )
    }
}

parse_expression_test! {
    field_access,
    "x.y.z",
    |state| {
        FieldExpr::new_e(
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
            FieldExpr::new_e(
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
            FieldExpr::new_e(
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
        FieldExpr::new_e(
            CallExpr::new_e(
                FieldExpr::new_e(
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
                    LiteralExpr::Integer(1),
                    BinaryExpr::new_e(
                        BinaryOperator::Multiply,
                        LiteralExpr::Integer(2),
                        BinaryExpr::new_e(
                            BinaryOperator::Divide,
                            LiteralExpr::Integer(3),
                            LiteralExpr::Integer(4),
                        )
                    )
                ),
                LiteralExpr::Integer(5)
            ),
            BinaryExpr::new_e(
                BinaryOperator::NotEq,
                LiteralExpr::Integer(6),
                UnaryExpr::new_e(
                    UnaryOperator::Positive,
                    UnaryExpr::new_e(
                        UnaryOperator::BitwiseNot,
                        LiteralExpr::Integer(7),
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

parse_test! {
    basic_frag_function,
    "
@frag
fn my_frag_main(coord: vec2f): vec4f {
    vec4f(coord, 0, 1)
}",
    |state| vec![
        FunctionItem {
            attributes: vec![
                Attribute {
                    args: None,
                    path: Qpath(thin_vec![Ident::Keyword(Keyword::Frag)])
                }
            ],
            export: false,
            name: state.str_to_ident("my_frag_main"),
            parameters: vec![
                ParameterItem {
                    attributes: vec![],
                    name: state.str_to_ident("coord"),
                    explicit_type: WrittenTy::Vector(VectorTy {
                        size: DimCount::Two,
                        repr: VectorRepr::Float(None)
                    })
                }
            ],
            return_type: WrittenTy::Vector(VectorTy {
                size: DimCount::Four,
                repr: VectorRepr::Float(None)
            }),
            body: BlockExpr { statements: vec![BlockStatement::Expression(CallExpr {
                receiver: Expression::Access(Ident::Keyword(Keyword::Vec(VectorTy {
                    size: DimCount::Four,
                    repr: VectorRepr::Float(None)
                }))).into(),
                arguments: vec![
                    Expression::Access(state.str_to_ident("coord")),
                    LiteralExpr::Integer(0).into(),
                    LiteralExpr::Integer(1).into(),
                ]
            }.into())] }
        }.into()
    ]
}

parse_test! {
    what_the_lower_uses,
    "
# test
@frag
fn test(): mat2 {}
",
    |state| vec![
        FunctionItem {
            attributes: vec![
                Attribute {
                    args: None,
                    path: Qpath(thin_vec![Ident::Keyword(Keyword::Frag)])
                }
            ],
            export: false,
            name: state.str_to_ident("test"),
            parameters: vec![],
            return_type: WrittenTy::Matrix(MatrixTy {
                size: yuri_ast::MatrixDim::Short(DimCount::Two),
                repr: None
            }),
            body: BlockExpr { statements: vec![] }
        }.into()
    ]
}

parse_test! {
    example_yuri,
    include_str!("../../example.yuri"),
    |state| vec![
        TypeAliasItem {
            export: false,
            name: state.str_to_ident("VertexOutput"),
            // {{ @vert.pos out: vec4f, coord: vec2f }}
            type_attributes: vec![],
            aliases: CompoundTy {
                fields: vec![
                    CompoundTyField {
                        attributes: vec![Attribute {
                            path: Qpath(thin_vec![
                                state.str_to_ident("vert"),
                                state.str_to_ident("pos"),
                            ]),
                            args: None
                        }],
                        name: state.str_to_ident("out"),
                        field_ty: WRITTEN_VEC4F
                    },
                    CompoundTyField {
                        attributes: vec![],
                        name: state.str_to_ident("coord"),
                        field_ty: WRITTEN_VEC2F
                    }
                ]
            }.into()
        }.into(),
        FunctionItem {
            attributes: vec![
                Attribute {
                    args: None,
                    path: Qpath(thin_vec![Ident::Keyword(Keyword::Vert)])
                }
            ],
            export: false,
            name: state.str_to_ident("my_vert_main"),
            parameters: vec![
                ParameterItem {
                    attributes: vec![],
                    name: state.str_to_ident("pos"),
                    explicit_type: WRITTEN_VEC3F
                },
                ParameterItem {
                    attributes: vec![],
                    name: state.str_to_ident("coord"),
                    explicit_type: WRITTEN_VEC2F
                }
            ],
            return_type: WrittenTy::Alias(Qpath::single(state.str_to_ident("VertexOutput"))),
            body: BlockExpr { statements: vec![BlockStatement::Expression(CompoundExpr {
                fields: vec![
                    CompoundExprField {
                        attributes: vec![],
                        name: state.str_to_ident("out"),
                        written_ty: None,
                        value: Some(CallExpr {
                            receiver: Expression::Access(Ident::Keyword(Keyword::Vec(VectorTy {
                                size: DimCount::Four,
                                repr: VectorRepr::Float(None)
                            }))).into(),
                            arguments: vec![
                                Expression::Access(state.str_to_ident("pos")),
                                LiteralExpr::Integer(1).into(),
                            ]
                        }.into())
                    },
                    CompoundExprField {
                        attributes: vec![],
                        name: state.str_to_ident("pos"),
                        written_ty: None,
                        value: None
                    },
                    CompoundExprField {
                        attributes: vec![],
                        name: state.str_to_ident("coord"),
                        written_ty: None,
                        value: None
                    }
                ]
            }.into())] }
        }.into(),
        FunctionItem {
            attributes: vec![
                Attribute {
                    args: None,
                    path: Qpath(thin_vec![Ident::Keyword(Keyword::Frag)])
                }
            ],
            export: false,
            name: state.str_to_ident("my_frag_main"),
            parameters: vec![
                ParameterItem {
                    attributes: vec![],
                    name: state.str_to_ident("coord"),
                    explicit_type: WRITTEN_VEC2F
                }
            ],
            return_type: WRITTEN_VEC4F,
            body: BlockExpr { statements: vec![BlockStatement::Expression(CallExpr {
                receiver: Expression::Access(Ident::Keyword(Keyword::Vec(VectorTy {
                    size: DimCount::Four,
                    repr: VectorRepr::Float(None)
                }))).into(),
                arguments: vec![
                    Expression::Access(state.str_to_ident("coord")),
                    LiteralExpr::Integer(0).into(),
                    LiteralExpr::Integer(1).into(),
                ]
            }.into())] }
        }.into(),
    ]
}

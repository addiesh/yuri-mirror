use crate::token::LiteralKind::{Float, Int};
use crate::token::TokenKind;
use crate::token::TokenKind::*;
use crate::token::{Base, Token};
use crate::tokenize;

macro_rules! lex_test {
    ($input:expr, $expected:expr) => {{
        let expected: [(TokenKind, u32); _] = $expected;
        let actual: Box<[Token]> = tokenize($input).collect();
        assert_eq!(*actual, expected.map(|(a, b)| Token::new(a, b)));
    }};
}

#[test]
fn numerics() {
    // integer
    lex_test!("12345", [(Literal(Int(Base::Decimal)), 5)]);
    // float
    lex_test!("0.123", [(Literal(Float), 5)]);
    // hex
    lex_test!("0x123", [(Literal(Int(Base::Hexadecimal)), 5)]);
    // binary
    lex_test!("0b101", [(Literal(Int(Base::Binary)), 5)]);
    // float with (unsigned, assumed positive) e notation
    lex_test!("1.2e3", [(Literal(Float), 5)]);
    // float with positive e notation
    lex_test!("1.2e+3", [(Literal(Float), 6)]);
    // float with negative e notation
    lex_test!("1.2e-3", [(Literal(Float), 6)]);
    // float without digits
    lex_test!("1.", [(Literal(Float), 2)]);
    // invalid (needs leading digit)
    lex_test!(".123", [(Dot, 1), (Literal(Int(Base::Decimal)), 3),]);
    // invalid (hex literals cannot have a fractional part)
    lex_test!(
        "0x12.3",
        [
            (Literal(Int(Base::Hexadecimal)), 4),
            (Dot, 1),
            (Literal(Int(Base::Decimal)), 1),
        ]
    );
    // invalid (hex with non-hex digit)
    lex_test!("0xEFG", [(Literal(Int(Base::Hexadecimal)), 4), (Ident, 1),]);
    // invalid (binary with non-binary digit)
    lex_test!(
        "0b012",
        [
            (Literal(Int(Base::Binary)), 4),
            (Literal(Int(Base::Decimal)), 1),
        ]
    );
    // invalid scientific notation (needs decimal before 'e')
    lex_test!("1e23", [(Literal(Int(Base::Decimal)), 1), (Ident, 3),]);
}

#[test]
fn sequences() {
    lex_test!("***", [(DoubleStar, 2), (Star, 1)]);
    lex_test!("* **", [(Star, 1), (Whitespace, 1), (DoubleStar, 2)]);
    lex_test!("{{{", [(OpenDoubleBrace, 2), (OpenBrace, 1)]);
    lex_test!(
        "{ {{",
        [(OpenBrace, 1), (Whitespace, 1), (OpenDoubleBrace, 2)]
    );
}
#[test]
fn whitespace() {
    // LF
    lex_test!("# comment\n", [(LineComment, 9), (Newline, 1)]);
    // CRLF
    lex_test!("# comment\r\n", [(LineComment, 9), (Newline, 2)]);
    // weird case with an extra carriage return
    lex_test!(
        "# comment\r\r\n",
        [(LineComment, 9), (Newline, 1), (Newline, 2)]
    );
    // more edge cases
    lex_test!(
        "# comment\r\r\n",
        [(LineComment, 9), (Newline, 1), (Newline, 2)]
    );

    lex_test!("a\nb", [(Ident, 1), (Newline, 1), (Ident, 1)]);
    lex_test!("a\r\nb", [(Ident, 1), (Newline, 2), (Ident, 1)]);
}

#[test]
fn big_example_program() {
    lex_test!(
        concat!(
            "# Example shader\n",
            "@frag\n",
            "fn main(input: f2): f4 {\r\n",
            "\tlet color = f4(input, 0.0, 1.0)\n",
            "    color\n",
            "}\n",
        ),
        [
            // # Example shader
            (LineComment, 16),
            (Newline, 1),
            // @frag
            (At, 1),
            (Ident, 4),
            (Newline, 1),
            // fn
            (Ident, 2),
            (Whitespace, 1),
            // main
            (Ident, 4),
            (OpenParen, 1),
            // input:
            (Ident, 5),
            (Colon, 1),
            (Whitespace, 1),
            // f2
            (Ident, 2),
            (CloseParen, 1),
            (Colon, 1),
            (Whitespace, 1),
            // f4
            (Ident, 2),
            (Whitespace, 1),
            (OpenBrace, 1),
            (Newline, 2),
            (Whitespace, 1),
            // let
            (Ident, 3),
            (Whitespace, 1),
            // color
            (Ident, 5),
            (Whitespace, 1),
            (Eq, 1),
            (Whitespace, 1),
            (Ident, 2),
            (OpenParen, 1),
            // input
            (Ident, 5),
            (Comma, 1),
            (Whitespace, 1),
            // 0.0
            (Literal(Float), 3),
            (Comma, 1),
            (Whitespace, 1),
            // 1.0
            (Literal(Float), 3),
            (CloseParen, 1),
            (Newline, 1),
            // tail expression
            (Whitespace, 4),
            // color
            (Ident, 5),
            (Newline, 1),
            (CloseBrace, 1),
            (Newline, 1),
        ]
    )
}

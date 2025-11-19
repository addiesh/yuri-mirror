use std::f64;

use smallvec::SmallVec;
use thin_vec::thin_vec;
use yuri_common::{BinaryOperator, UnaryOperator};
use yuri_lexer::TokenKind;
use yuri_lexer::token::{Base, LiteralKind};

use crate::error::ParseTry;
use crate::expression::{
    BinaryExpression, CallExpression, Expression, LiteralExpression, UnaryExpression,
};
use crate::parse::ParseState;
use crate::{Ident, Keyword, ParseError, Qpath};

/// Define a function to consume a left-associative binary expression.
/// I don't particularly like macros in Rust, but this felt like a reasonable use case.
macro_rules! lass_bin_take {
    ($this:ident, $next:ident, $body:expr) => {
        pub fn $this(&mut self) -> Result<Expression, ParseError> {
            let mut expr = self.$next()?;

            loop {
                self.take_whitespace();

                let fun: fn(this: &mut Self) -> Result<Option<BinaryOperator>, ParseError> = $body;

                let Some(operator) = (fun(self))? else {
                    println!(concat!("didn't take ", stringify!($this)));
                    break;
                };

                self.take_whitespace();

                let right = self.$this()?;
                expr = Expression::Binary(BinaryExpression {
                    operator,
                    left: Box::new(expr),
                    right: Box::new(right),
                });
            }

            Ok(expr)
        }
    };
}

impl<'src> ParseState<'src, '_> {
    pub fn expression(&mut self) -> Result<Expression, ParseError> {
        self.expr_logic_or()
    }

    lass_bin_take!(expr_logic_or, expr_logic_xor, |this| {
        let Some(tok) = this.peek() else {
            return Ok(None);
        };
        if tok.kind == TokenKind::Ident {
            let ident = this.token_to_ident(tok);
            if let Ident::Keyword(Keyword::Or) = ident {
                this.skip();
                return Ok(Some(BinaryOperator::LogicOr));
            }
        }
        Ok(None)
    });

    lass_bin_take!(expr_logic_xor, expr_logic_and, |this| {
        let Some(tok) = this.peek() else {
            return Ok(None);
        };
        if tok.kind == TokenKind::Ident {
            let ident = this.token_to_ident(tok);
            if let Ident::Keyword(Keyword::Xor) = ident {
                this.skip();
                return Ok(Some(BinaryOperator::LogicXor));
            }
        }
        Ok(None)
    });

    lass_bin_take!(expr_logic_and, expr_compare, |this| {
        let Some(tok) = this.peek() else {
            return Ok(None);
        };
        if tok.kind == TokenKind::Ident {
            let ident = this.token_to_ident(tok);
            if let Ident::Keyword(Keyword::And) = ident {
                this.skip();
                return Ok(Some(BinaryOperator::LogicAnd));
            }
        }
        Ok(None)
    });

    pub fn expr_compare(&mut self) -> Result<Expression, ParseError> {
        let mut expr = self.expr_bit_or()?;

        self.take_whitespace();

        let Some(operator) = self.switch_seq(&[
            (BinaryOperator::Eq, &[TokenKind::Eq, TokenKind::Eq]),
            (BinaryOperator::NotEq, &[TokenKind::Bang, TokenKind::Eq]),
            (BinaryOperator::GtEq, &[TokenKind::Gt, TokenKind::Eq]),
            (BinaryOperator::LtEq, &[TokenKind::Lt, TokenKind::Eq]),
            (BinaryOperator::Gt, &[TokenKind::Gt]),
            (BinaryOperator::Lt, &[TokenKind::Lt]),
        ]) else {
            println!("didn't take comparison");
            return Ok(expr);
        };

        self.take_whitespace();

        let right = self.expr_compare()?;
        expr = Expression::Binary(BinaryExpression {
            operator,
            left: Box::new(expr),
            right: Box::new(right),
        });

        Ok(expr)
    }

    lass_bin_take!(expr_bit_or, expr_bit_xor, |this| Ok(this
        .take_seq(&[TokenKind::Pipe])
        .then_some(BinaryOperator::BitOr)));

    lass_bin_take!(expr_bit_xor, expr_bit_and, |this| Ok(this
        .take_seq(&[TokenKind::Caret])
        .then_some(BinaryOperator::BitXor)));

    lass_bin_take!(expr_bit_and, expr_shift, |this| Ok(this
        .take_seq(&[TokenKind::Amp])
        .then_some(BinaryOperator::BitAnd)));

    lass_bin_take!(expr_shift, expr_sum, |this| Ok(this.switch_seq(&[
        (BinaryOperator::ShiftLeft, &[TokenKind::Lt, TokenKind::Lt]),
        (BinaryOperator::ShiftRight, &[TokenKind::Gt, TokenKind::Gt]),
    ])));

    lass_bin_take!(expr_sum, expr_product, |this| Ok(this.switch_seq(&[
        (BinaryOperator::Add, &[TokenKind::Plus]),
        (BinaryOperator::Sub, &[TokenKind::Minus]),
    ])));

    lass_bin_take!(expr_product, expr_exponent, |this| Ok(this.switch_seq(&[
        (BinaryOperator::Multiply, &[TokenKind::Star]),
        (BinaryOperator::Divide, &[TokenKind::Slash]),
        (BinaryOperator::Remainder, &[TokenKind::Percent]),
    ])));

    lass_bin_take!(expr_exponent, expr_unary, |this| Ok(this
        .take_seq(&[TokenKind::DoubleStar])
        .then_some(BinaryOperator::Exponent)));

    pub fn expr_unary(&mut self) -> Result<Expression, ParseError> {
        let uop = self.peek().eof()?;
        if matches!(
            uop.kind,
            TokenKind::Plus | TokenKind::Minus | TokenKind::Bang | TokenKind::Tilde
        ) {
            self.skip();
            let operator = match uop.kind {
                TokenKind::Plus => UnaryOperator::Positive,
                TokenKind::Minus => UnaryOperator::Negative,
                TokenKind::Bang => UnaryOperator::LogicalNot,
                TokenKind::Tilde => UnaryOperator::BitwiseNot,
                _ => unreachable!(),
            };
            self.take_whitespace();
            let value = Box::new(self.expr_unary()?);
            self.take_whitespace();
            Ok(Expression::Unary(UnaryExpression { operator, value }))
        } else {
            self.expr_call()
        }
    }

    pub fn expr_call(&mut self) -> Result<Expression, ParseError> {
        let expr = self.expr_path()?;
        return Ok(expr);
        self.take_whitespace();
        let Some(possible) = self.peek() else {
            return Ok(expr);
        };
        if possible.kind == TokenKind::OpenParen {
            let mut args = SmallVec::<[Expression; 4]>::new();
            self.skip();
            self.take_whitespace();

            todo!();
            let arg = self.expression()?;

            self.take_whitespace();
            self.take().eof()?.expect(TokenKind::CloseParen)?;

            Ok(Expression::FunctionalCall(CallExpression {
                receiver: Box::new(expr),
                arguments: args.into_vec(),
            }))
        } else {
            self.expr_call()
        }
    }

    // aka. field
    pub fn expr_path(&mut self) -> Result<Expression, ParseError> {
        println!("TODO: implement path/field expression");
        self.expr_maxprec()
    }

    pub fn expr_maxprec(&mut self) -> Result<Expression, ParseError> {
        let tok = self.take().eof()?;
        self.take_whitespace();

        match tok.kind {
            // either:
            // 1. "if" or some other funky expression
            // 2. a variable
            // 3. true/false/nan/infinity/etc.
            TokenKind::Ident => {
                use LiteralExpression as LitExp;
                println!("TODO if/loop/variable expression: {tok:?}");
                let ident = self.token_to_ident(tok);
                Ok(match ident {
                    Ident::Id(_) => Expression::Variable(Qpath(thin_vec![ident])),
                    Ident::Keyword(Keyword::If) => Expression::Unimplemented,
                    Ident::Keyword(Keyword::True | Keyword::False) => Expression::Unimplemented,
                    Ident::Keyword(Keyword::NaN) => LitExp::Decimal(f64::NAN).into(),
                    Ident::Keyword(Keyword::Inf) => LitExp::Decimal(f64::INFINITY).into(),
                    Ident::Keyword(
                        Keyword::Loop
                        | Keyword::Filter
                        | Keyword::Flatten
                        | Keyword::Fold
                        | Keyword::Reverse
                        | Keyword::Append
                        | Keyword::Prepend
                        | Keyword::Join
                        | Keyword::Map,
                    ) => Expression::Unimplemented,
                    Ident::Keyword(_) => return Err(ParseError::UnexpectedToken(tok)),
                })
            }
            // number
            TokenKind::Literal(literal_kind) => {
                use LiteralExpression::*;
                let tks = self.str_from_token(tok);
                Ok(Expression::Literal(match literal_kind {
                    LiteralKind::Int(Base::Decimal) => Number(tks.parse().unwrap()),
                    LiteralKind::Int(_) => Integer(tks.parse().unwrap()),
                    LiteralKind::Float => Decimal(tks.parse().unwrap()),
                }))
            }
            // group
            TokenKind::OpenParen => {
                // self.take_delimited(TokenKind::OpenParen, TokenKind::CloseParen);
                self.take_whitespace();
                let expr = self.expression()?;
                self.take_whitespace();
                self.take().eof()?.expect(TokenKind::CloseParen)?;
                Ok(expr)
            }
            // block
            TokenKind::OpenBrace => todo!("expression block"),
            TokenKind::OpenDoubleBrace => todo!("object block"),
            TokenKind::OpenBracket => todo!("array init"),
            _ => Ok(Expression::Unimplemented),
            // _ => Err(ParseError::UnexpectedToken(tok)),
        }
    }
}

//! program = module_block;
//!
//! module_block = {
//!     function_item
//!     | type_alias_item
//!     | module_item
//!     | variable_item
//!     | import_item
//! };
//!
//! module_item =
//!     [ "export" ],
//!     "module",
//!     Ident,
//!     "{",
//!         module_block,
//!     "}";
//!
//! qpath = { Ident | (Ident, ".", qpath) };
//!
//! attribute = "@", qpath;
//!
//! type = qpath | array_type | compound_type;
//!
//! array_type = "[", type, ";", expression, "]";
//!
//! compound_type_field = { attribute }, Ident, ":", type;
//! compound_type_fields = compound_type_field, [",", compound_type_fields];
//! compound_type =
//!     "{{",
//!     ( compound_type_fields, [","] ),
//!     "}}";
//!
//! variable_item = ["export], "let", Ident, "=", expression;
//!
//! type_alias_item = ["export], "type", Ident, "=", type;
//!
//! expression = expression_1;
//! expression_1 = expression_2, { "or", expression_2 };
//! expression_2 = expression_3, { "and", expression_3 };
//! expression_3 = expression_4, [ ("==" | "!=" | ">" | "<" | "<=" | ">="), expression_4 ];
//! expression_4 = expression_5, { "|", expression_5 };
//! expression_5 = expression_6, { "^", expression_6 };
//! expression_6 = expression_7, { "&", expression_7 };
//! expression_7 = expression_8, { ("<<" | ">>"), expression_8 };
//! expression_8 = expression_9, { ("+" | "-"), expression_9 };
//! expression_9 = expression_a, { ("*" | "/" | "%"), expression_a };
//! expression_a = expression_b, { "**", expression_b };
//! expression_b = ( ("!" | "~" | "+" | "-"), expression_b) | expression_c;
//! expression_c =
//!     ( "(", expression, ")" )
//!     | literal
//!     | call_expression
//!     | if_expression
//!     | array_expression
//!     | compound_expression
//!     | expression_block;
//!
//!
//! compound_expression_field = Ident, "=", expression;
//! compound_expression_fields = compound_expression_field, [",", compound_expression_fields];
//! compound_expression =
//!     "{{",
//!     ( compound_expression_fields, [","] ),
//!     "}}";
//!
//! function_param = { attribute }, Ident, ":", type;
//! function_params = function_param, [",", function_params]
//! function_item =
//!     { attribute },
//!     [ "export" ],
//!     "fn",
//!     Ident,
//!     "(",
//!     ( function_params, [","] )
//!     ")",
//!     ":",
//!     type,
//!     expression_block;
//!
//!
//! expression_block = {
//!     function_item
//!     | type_alias_item
//!     | variable_item
//!     | import_item
//!     | expression
//! };

use yuri_lexer::token::{Token, TokenKind};

use crate::expression::{BinaryOperator, Expression};
use crate::item::OuterDeclaration;
use crate::{ParseError, ParseState, Qpath};

pub fn parse_outer_declaration(input: ParseState) -> Result<OuterDeclaration, ParseError> {
    todo!()
}

pub fn parse_qpath(input: ParseState) -> Result<Qpath, ParseError> {
    todo!()
}

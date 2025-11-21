use std::collections::HashMap;
use std::ops::Deref;

use thin_vec::ThinVec;
use yuri_common::{DimensionCount, FloatBits, IntBits};
use yuri_lexer::token::TokenKind;

use crate::error::ParseError;
use crate::item::OuterDeclaration;

pub use parse::parse_all;

pub mod error;
pub mod expression;
pub mod item;
pub mod parse;
pub mod types;

pub type Ast = Vec<OuterDeclaration>;

/// "Full" token
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub struct TokenF {
    pub kind: TokenKind,
    pub byte_offset: u32,
    pub len: u32,
}

#[derive(Default)]
pub struct ParseStorage {
    // TODO: figure out how to share resources better here.
    // this storage was meant for the parser only, but is quickly integrating into the compiler.
    ident_list: Vec<String>,
    ident_set: HashMap<String, usize>,
}

impl ParseStorage {
    pub fn to_ident(&mut self, string: &str) -> Ident {
        if let Some(ident) = Keyword::try_from_str(string) {
            Ident::Keyword(ident)
        } else if let Some(index) = self.ident_set.get(string) {
            Ident::Id(*index)
        } else {
            let index = self.ident_list.len();
            self.ident_list.push(string.to_owned());
            self.ident_set.insert(string.to_owned(), index);
            Ident::Id(index)
        }
    }

    // pub fn resolve(&'src self, ident: &Ident) -> Option<&'src str> {
    pub fn resolve(&self, ident: &Ident) -> Option<&str> {
        match ident {
            Ident::Id(index) => self.ident_list.get(*index).map(Deref::deref),
            Ident::Keyword(keyword) => Some(keyword.as_str()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Ident {
    Id(usize),
    Keyword(Keyword),
}

#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct Qpath(pub ThinVec<Ident>);

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum Keyword {
    Import,
    Export,
    Fn,
    Let,
    Type,
    Module,
    Break,
    Continue,
    Else,
    If,
    Return,
    And,
    Or,
    Xor,
    Loop,
    Fold,
    Reverse,
    Map,
    Flatten,
    Filter,
    Append,
    Prepend,
    Join,

    True,
    False,
    NaN,
    Inf,

    Not,
    Vec {
        elements: DimensionCount,
        repr: VectorRepr,
    },
    Mat {
        dimensions: MatrixDimensions,
        bitsize: Option<FloatBits>,
    },
    Int,
    Float,
    Sampler,
    Tex1,
    Tex2,
    Tex2Array,
    Tex3,
    TexCube,
    TexCubeArray,
}

impl Keyword {
    pub const fn as_str<'a>(self) -> &'a str {
        use DimensionCount::*;
        use FloatBits::*;
        use IntBits::*;
        use VectorRepr::*;

        match self {
            Self::Import => "import",
            Self::Export => "export",
            Self::Fn => "fn",
            Self::Let => "let",
            Self::Type => "type",
            Self::Module => "module",
            Self::Break => "break",
            Self::Continue => "continue",
            Self::Else => "else",
            Self::If => "if",
            Self::Return => "return",
            Self::And => "and",
            Self::Or => "or",
            Self::Xor => "xor",
            Self::Loop => "loop",
            Self::Fold => "fold",
            Self::Reverse => "reverse",
            Self::Map => "map",
            Self::Flatten => "flatten",
            Self::Filter => "filter",
            Self::Append => "append",
            Self::Prepend => "prepend",
            Self::Join => "join",
            Self::True => "true",
            Self::False => "false",
            Self::NaN => "NaN",
            Self::Inf => "Inf",
            Self::Not => "not",
            Self::Int => "int",
            Self::Float => "float",
            Self::Sampler => "sampler",
            Self::Tex1 => "tex1",
            Self::Tex2 => "tex2",
            Self::Tex2Array => "tex2array",
            Self::Tex3 => "tex3",
            Self::TexCube => "texcube",
            Self::TexCubeArray => "texcubearray",
            Self::Vec { elements, repr } => match (elements, repr) {
                (Two, Unsigned(None)) => "vec2u",
                (Three, Unsigned(None)) => "vec3u",
                (Four, Unsigned(None)) => "vec4u",

                (Two, Signed(None)) => "vec2i",
                (Three, Signed(None)) => "vec3i",
                (Four, Signed(None)) => "vec4i",

                (Two, Float(None)) => "vec2f",
                (Three, Float(None)) => "vec3f",
                (Four, Float(None)) => "vec4f",

                (Two, Unsigned(Some(int_bit_size))) => match int_bit_size {
                    Int8 => "vec2u8",
                    Int16 => "vec2u16",
                    Int32 => "vec2u32",
                    Int64 => "vec2u64",
                },
                (Two, Signed(Some(int_bit_size))) => match int_bit_size {
                    Int8 => "vec2f8",
                    Int16 => "vec2f16",
                    Int32 => "vec2f32",
                    Int64 => "vec2f64",
                },
                (Two, Float(Some(float_bit_size))) => match float_bit_size {
                    Float16 => "vec2f16",
                    Float32 => "vec2f32",
                    Float64 => "vec2f64",
                },
                (Three, Unsigned(Some(int_bit_size))) => match int_bit_size {
                    Int8 => "vec3u8",
                    Int16 => "vec3u16",
                    Int32 => "vec3u32",
                    Int64 => "vec3u64",
                },
                (Three, Signed(Some(int_bit_size))) => match int_bit_size {
                    Int8 => "ve3iu8",
                    Int16 => "vec3i16",
                    Int32 => "vec3i32",
                    Int64 => "vec3i64",
                },
                (Three, Float(Some(float_bit_size))) => match float_bit_size {
                    Float16 => "vec3f16",
                    Float32 => "vec3f32",
                    Float64 => "vec3f64",
                },
                (Four, Unsigned(Some(int_bit_size))) => match int_bit_size {
                    Int8 => todo!(),
                    Int16 => todo!(),
                    Int32 => todo!(),
                    Int64 => todo!(),
                },
                (Four, Signed(Some(int_bit_size))) => match int_bit_size {
                    Int8 => todo!(),
                    Int16 => todo!(),
                    Int32 => todo!(),
                    Int64 => todo!(),
                },
                (Four, Float(Some(float_bit_size))) => match float_bit_size {
                    Float16 => todo!(),
                    Float32 => todo!(),
                    Float64 => todo!(),
                },
            },
            Self::Mat {
                dimensions,
                bitsize,
            } => match dimensions {
                MatrixDimensions::Short(Two) => match bitsize {
                    None => "mat2",
                    Some(Float16) => "mat2f16",
                    Some(Float32) => "mat2f32",
                    Some(Float64) => "mat2f64",
                },
                MatrixDimensions::Short(Three) => match bitsize {
                    None => "mat3",
                    Some(Float16) => "mat3f16",
                    Some(Float32) => "mat3f32",
                    Some(Float64) => "mat3f64",
                },
                MatrixDimensions::Short(Four) => match bitsize {
                    None => "mat4",
                    Some(Float16) => "mat4f16",
                    Some(Float32) => "mat4f32",
                    Some(Float64) => "mat4f64",
                },
                MatrixDimensions::Long { columns, rows } => match (columns, rows) {
                    (Two, Two) => match bitsize {
                        None => "mat2x2",
                        Some(Float16) => "mat2x2f16",
                        Some(Float32) => "mat2x2f32",
                        Some(Float64) => "mat2x2f64",
                    },
                    (Two, Three) => match bitsize {
                        None => "mat2x3",
                        Some(Float16) => "mat2x3f16",
                        Some(Float32) => "mat2x3f32",
                        Some(Float64) => "mat2x3f64",
                    },
                    (Two, Four) => match bitsize {
                        None => "mat4",
                        Some(Float16) => "mat4f16",
                        Some(Float32) => "mat4f32",
                        Some(Float64) => "mat4f64",
                    },
                    (Three, Two) => match bitsize {
                        None => "mat3x2",
                        Some(Float16) => "mat3x2f16",
                        Some(Float32) => "mat3x2f32",
                        Some(Float64) => "mat3x2f64",
                    },
                    (Three, Three) => match bitsize {
                        None => "mat3x3",
                        Some(Float16) => "mat3x3f16",
                        Some(Float32) => "mat3x3f32",
                        Some(Float64) => "mat3x3f64",
                    },
                    (Three, Four) => match bitsize {
                        None => "mat3x4",
                        Some(Float16) => "mat3x4f16",
                        Some(Float32) => "mat3x4f32",
                        Some(Float64) => "mat3x4f64",
                    },
                    (Four, Two) => match bitsize {
                        None => "mat4x2",
                        Some(Float16) => "mat4x2f16",
                        Some(Float32) => "mat4x2f32",
                        Some(Float64) => "mat4x2f64",
                    },
                    (Four, Three) => match bitsize {
                        None => "mat4x3",
                        Some(Float16) => "mat4x3f16",
                        Some(Float32) => "mat4x3f32",
                        Some(Float64) => "mat4x3f64",
                    },
                    (Four, Four) => match bitsize {
                        None => "mat4x4",
                        Some(Float16) => "mat4x4f16",
                        Some(Float32) => "mat4x4f32",
                        Some(Float64) => "mat4x4f64",
                    },
                },
            },
        }
    }

    // PartialEq isn't const :/
    pub fn try_from_str(value: &str) -> Option<Self> {
        Some(match value {
            "import" => Self::Import,
            "export" => Self::Export,
            "fn" => Self::Fn,
            "let" => Self::Let,
            "type" => Self::Type,
            "module" => Self::Module,
            "break" => Self::Break,
            "continue" => Self::Continue,
            "else" => Self::Else,
            "if" => Self::If,
            "return" => Self::Return,
            "and" => Self::And,
            "or" => Self::Or,
            "xor" => Self::Xor,
            "loop" => Self::Loop,
            "fold" => Self::Fold,
            "reverse" => Self::Reverse,
            "map" => Self::Map,
            "flatten" => Self::Flatten,
            "filter" => Self::Filter,
            "append" => Self::Append,
            "prepend" => Self::Prepend,
            "join" => Self::Join,
            "not" => Self::Not,
            "int" => Self::Int,
            "float" => Self::Float,
            "sampler" => Self::Sampler,
            "tex1" => Self::Tex1,
            "tex2" => Self::Tex2,
            "tex2array" => Self::Tex2Array,
            "tex3" => Self::Tex3,
            "texcube" => Self::TexCube,
            "texcubearray" => Self::TexCubeArray,
            _ => return None,
        })
    }
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum VectorRepr {
    Unsigned(Option<IntBits>),
    Signed(Option<IntBits>),
    Float(Option<FloatBits>),
}

#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum MatrixDimensions {
    Short(DimensionCount),
    Long {
        columns: DimensionCount,
        rows: DimensionCount,
    },
}

use std::borrow::Cow;
use std::fmt::Display;

use rustc_hash::FxHashMap;
use thin_vec::ThinVec;
use yuri_common::{DimensionCount, FloatBits, IntBits};

use crate::{
    item::OuterDeclaration,
    types::{MatrixTy, VectorTy},
};

pub mod expression;
pub mod item;
pub mod types;

pub type Ast = Vec<OuterDeclaration>;

#[derive(Default)]
pub struct InStorage {
    ident_list: Vec<String>,
    ident_set: FxHashMap<String, usize>,
}

impl InStorage {
    pub fn to_ident(&mut self, string: &str) -> Ident {
        if let Some(ident) = Keyword::try_from_str(string) {
            Ident::Keyword(ident)
        } else if let Some(index) = self.ident_set.get(string) {
            Ident::Id(*index as u32)
        } else {
            let index = self.ident_list.len();
            self.ident_list.push(string.to_owned());
            self.ident_set.insert(string.to_owned(), index);
            Ident::Id(index as u32)
        }
    }

    // pub fn resolve(&'src self, ident: &Ident) -> Option<&'src str> {
    pub fn resolve(&self, ident: &Ident) -> Option<Cow<'_, str>> {
        match ident {
            Ident::Id(index) => self.ident_list.get(*index as usize).map(Into::into),
            Ident::Keyword(keyword) => Some(keyword.as_str()),
        }
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub enum Ident {
    Id(u32),
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

    /// Can't call this "Self" so this is good enough
    Myself,
    True,
    False,
    Nan,
    Inf,
    Pi,
    Tau,

    Not,
    // There are a lot of variations to the vector/matrix keywords, so we represent them using the Ty structs.
    Vec(VectorTy),
    Mat(MatrixTy),

    Float(FloatBits),
    Unsigned(IntBits),
    Signed(IntBits),

    Sampler,
    Tex1,
    Tex2,
    Tex2Array,
    Tex3,
    TexCube,
    TexCubeArray,
}

impl Keyword {
    // TODO: this could feasibly be const, but I need format strings.
    //       without pulling in const_format (which I don't feel like doing),
    //       this would be an abhorrent nightmare of match expressions.
    pub fn as_str<'a>(self) -> Cow<'a, str> {
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
            Self::Not => "not",

            Self::Loop => "loop",
            Self::Fold => "fold",
            Self::Reverse => "reverse",
            Self::Map => "map",
            Self::Flatten => "flatten",
            Self::Filter => "filter",
            Self::Append => "append",
            Self::Prepend => "prepend",
            Self::Join => "join",

            Self::Myself => "self",
            Self::True => "true",
            Self::False => "false",
            Self::Nan => "NaN",
            Self::Inf => "Inf",
            Self::Pi => "pi",
            Self::Tau => "tau",

            Self::Signed(IntBits::Int8) => "i8",
            Self::Signed(IntBits::Int16) => "i16",
            Self::Signed(IntBits::Int32) => "i32",
            Self::Signed(IntBits::Int64) => "i64",
            Self::Unsigned(IntBits::Int8) => "ui8",
            Self::Unsigned(IntBits::Int16) => "u16",
            Self::Unsigned(IntBits::Int32) => "u32",
            Self::Unsigned(IntBits::Int64) => "u64",
            Self::Float(FloatBits::Float16) => "f16",
            Self::Float(FloatBits::Float32) => "f32",
            Self::Float(FloatBits::Float64) => "f64",
            Self::Sampler => "sampler",
            Self::Tex1 => "tex1",
            Self::Tex2 => "tex2",
            Self::Tex2Array => "tex2array",
            Self::Tex3 => "tex3",
            Self::TexCube => "texcube",
            Self::TexCubeArray => "texcubearray",

            Self::Vec(VectorTy { size, repr }) => {
                let pref = format_args!("vec{size}");
                return match repr {
                    VectorRepr::Unsigned(None) => format!("{pref}u"),
                    VectorRepr::Signed(None) => format!("{pref}i"),
                    VectorRepr::Float(None) => format!("{pref}f"),
                    VectorRepr::Unsigned(Some(int_bits)) => format!("{pref}u{int_bits}"),
                    VectorRepr::Signed(Some(int_bits)) => format!("{pref}i{int_bits}"),
                    VectorRepr::Float(Some(float_bits)) => format!("{pref}f{float_bits}"),
                }
                .into();
            }
            Self::Mat(MatrixTy { size, repr }) => {
                let pref = format_args!("mat{size}");
                return match repr {
                    Some(bits) => format!("{pref}f{bits}"),
                    None => format!("{pref}"),
                }
                .into();
            }
        }
        .into()
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

            "if" => Self::If,
            "else" => Self::Else,

            "return" => Self::Return,

            "and" => Self::And,
            "or" => Self::Or,
            "xor" => Self::Xor,
            "not" => Self::Not,

            "loop" => Self::Loop,
            "fold" => Self::Fold,
            "reverse" => Self::Reverse,
            "map" => Self::Map,
            "flatten" => Self::Flatten,
            "filter" => Self::Filter,
            "append" => Self::Append,
            "prepend" => Self::Prepend,
            "join" => Self::Join,

            "self" => Self::Myself,
            "true" => Self::True,
            "false" => Self::False,
            "inf" => Self::Inf,
            "nan" => Self::Nan,
            "pi" => Self::Pi,
            "tau" => Self::Tau,

            "i8" => Self::Signed(IntBits::Int8),
            "i16" => Self::Signed(IntBits::Int16),
            "i32" => Self::Signed(IntBits::Int32),
            "i64" => Self::Signed(IntBits::Int64),
            "ui8" => Self::Unsigned(IntBits::Int8),
            "u16" => Self::Unsigned(IntBits::Int16),
            "u32" => Self::Unsigned(IntBits::Int32),
            "u64" => Self::Unsigned(IntBits::Int64),
            "f16" => Self::Float(FloatBits::Float16),
            "f32" => Self::Float(FloatBits::Float32),
            "f64" => Self::Float(FloatBits::Float64),

            "sampler" => Self::Sampler,
            "tex1" => Self::Tex1,
            "tex2" => Self::Tex2,
            "tex2array" => Self::Tex2Array,
            "tex3" => Self::Tex3,
            "texcube" => Self::TexCube,
            "texcubearray" => Self::TexCubeArray,

            _ => {
                if value.starts_with("vec") && value.len() <= 7 {
                    eprintln!("Can't parse vector typenames yet! (given \"{value}\")");
                    todo!("parse vector typenames");
                }
                if value.starts_with("mat") && value.len() <= 9 {
                    eprintln!("Can't parse matrix typenames yet! (given \"{value}\")");
                    todo!("parse matrix typenames");
                };
                return None;
            }
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

impl Display for MatrixDimensions {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MatrixDimensions::Short(dimension_count) => {
                write!(f, "{dimension_count}")
            }
            MatrixDimensions::Long { columns, rows } => {
                write!(f, "{columns}x{rows}")
            }
        }
    }
}

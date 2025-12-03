use std::borrow::Cow;
use std::fmt::{Debug, Display};

use rustc_hash::FxHashMap;
use thin_vec::ThinVec;
use yuri_common::{DimensionCount, FloatBits, IntBits};

use crate::item::OuterDeclaration;
use crate::types::{MatrixTy, VectorTy};

pub mod expression;
pub mod item;
pub mod types;

pub type Ast = Vec<OuterDeclaration>;

/// Interned string storage. Provides bidirectional lookup between strings and IDs.
/// The [Ident::Id] variant is used to lookup into the compiler storage.
#[derive(Default)]
pub struct InStorage {
    ident_list: Vec<String>,
    ident_set: FxHashMap<String, u32>,
}

impl InStorage {
    pub fn to_ident(&mut self, string: &str) -> Ident {
        if let Some(ident) = Keyword::try_from_str(string) {
            Ident::Keyword(ident)
        } else if let Some(index) = self.ident_set.get(string) {
            Ident::Id(*index)
        } else {
            let index = self.ident_list.len();
            self.ident_list.push(string.to_owned());
            self.ident_set.insert(string.to_owned(), index as u32);
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

/// A fully-qualified path, written in syntax as a dot-separated
#[derive(Debug, Clone, PartialEq)]
#[repr(transparent)]
pub struct Qpath(pub ThinVec<Ident>);

#[derive(Clone, Copy, Eq, PartialEq)]
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

    /// Can't call it "Self" but that's what this is.
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

    // I don't like these names, these are ripped straight from wgsl.
    // Will probably change in future.
    Sampler,
    Tex1,
    Tex2,
    Tex2Array,
    Tex3,
    TexCube,
    TexCubeArray,

    // Common builtins
    Frag,
    Vert,
    Pos,
    Cos,
    Sin,
    Tan,
    Cosh,
    Sinh,
    Tanh,
    Acos,
    Asin,
    Atan,
    Atan2,
}

impl Debug for Keyword {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "\"{}\"", self.as_str())
    }
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
            Self::Frag => "frag",
            Self::Vert => "vert",
            Self::Pos => "pos",
            Self::Cos => "cos",
            Self::Sin => "sin",
            Self::Tan => "tan",
            Self::Cosh => "cosh",
            Self::Sinh => "sinh",
            Self::Tanh => "tanh",
            Self::Acos => "acos",
            Self::Asin => "asin",
            Self::Atan => "atan",
            Self::Atan2 => "atan2",
        }
        .into()
    }

    // PartialEq isn't const :/
    #[rustfmt::skip]
    pub fn try_from_str(value: &str) -> Option<Self> {
        use MatrixDimensions::*;
        use DimensionCount::*;
        use FloatBits::*;
        use IntBits::*;
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

            "vec2f" =>  Self::Vec(VectorTy { size: Two, repr: VectorRepr::Float(None) }),
            "vec2f16" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Float(Some(Float16)) }),
            "vec2f32" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Float(Some(Float32)) }),
            "vec2f64" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Float(Some(Float64)) }),
            "vec2u" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Unsigned(None) }),
            "vec2u8" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Unsigned(Some(Int8)) }),
            "vec2u16" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Unsigned(Some(Int16)) }),
            "vec2u32" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Unsigned(Some(Int32)) }),
            "vec2u64" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Unsigned(Some(Int64)) }),
            "vec2i" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Signed(None) }),
            "vec2i8" =>  Self::Vec(VectorTy { size: Two, repr: VectorRepr::Signed(Some(Int8)) }),
            "vec2i16" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Signed(Some(Int16)) }),
            "vec2i32" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Signed(Some(Int32)) }),
            "vec2i64" => Self::Vec(VectorTy { size: Two, repr: VectorRepr::Signed(Some(Int64)) }),

            "vec3f" =>  Self::Vec(VectorTy { size: Three, repr: VectorRepr::Float(None) }),
            "vec3f16" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Float(Some(Float16)) }),
            "vec3f32" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Float(Some(Float32)) }),
            "vec3f64" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Float(Some(Float64)) }),
            "vec3u" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Unsigned(None) }),
            "vec3u8" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Unsigned(Some(Int8)) }),
            "vec3u16" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Unsigned(Some(Int16)) }),
            "vec3u32" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Unsigned(Some(Int32)) }),
            "vec3u64" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Unsigned(Some(Int64)) }),
            "vec3i" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Signed(None) }),
            "vec3i8" =>  Self::Vec(VectorTy { size: Three, repr: VectorRepr::Signed(Some(Int8)) }),
            "vec3i16" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Signed(Some(Int16)) }),
            "vec3i32" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Signed(Some(Int32)) }),
            "vec3i64" => Self::Vec(VectorTy { size: Three, repr: VectorRepr::Signed(Some(Int64)) }),

            "vec4f" =>  Self::Vec(VectorTy { size: Four, repr: VectorRepr::Float(None) }),
            "vec4f16" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Float(Some(Float16)) }),
            "vec4f32" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Float(Some(Float32)) }),
            "vec4f64" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Float(Some(Float64)) }),
            "vec4u" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Unsigned(None) }),
            "vec4u8" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Unsigned(Some(Int8)) }),
            "vec4u16" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Unsigned(Some(Int16)) }),
            "vec4u32" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Unsigned(Some(Int32)) }),
            "vec4u64" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Unsigned(Some(Int64)) }),
            "vec4i" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Signed(None) }),
            "vec4i8" =>  Self::Vec(VectorTy { size: Four, repr: VectorRepr::Signed(Some(Int8)) }),
            "vec4i16" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Signed(Some(Int16)) }),
            "vec4i32" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Signed(Some(Int32)) }),
            "vec4i64" => Self::Vec(VectorTy { size: Four, repr: VectorRepr::Signed(Some(Int64)) }),

            "mat2" => Self::Mat(MatrixTy { size: Short(Two), repr: None }),
            "mat2x2" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Two }, repr: None }),
            "mat2x3" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Three }, repr: None }),
            "mat2x4" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Four }, repr: None }),
            "mat3" => Self::Mat(MatrixTy { size: Short(Three), repr: None }),
            "mat3x2" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Two }, repr: None }),
            "mat3x3" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Three }, repr: None }),
            "mat3x4" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Four }, repr: None }),
            "mat4" => Self::Mat(MatrixTy { size: Short(Four), repr: None }),
            "mat4x2" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Two }, repr: None }),
            "mat4x3" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Three }, repr: None }),
            "mat4x4" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Four }, repr: None }),

            "mat2f16" => Self::Mat(MatrixTy { size: Short(Two), repr: Some(Float16) }),
            "mat2x2f16" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Two }, repr: Some(Float16) }),
            "mat2x3f16" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Three }, repr: Some(Float16) }),
            "mat2x4f16" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Four }, repr: Some(Float16) }),
            "mat3f16" => Self::Mat(MatrixTy { size: Short(Three), repr: Some(Float16) }),
            "mat3x2f16" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Two }, repr: Some(Float16) }),
            "mat3x3f16" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Three }, repr: Some(Float16) }),
            "mat3x4f16" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Four }, repr: Some(Float16) }),
            "mat4f16" => Self::Mat(MatrixTy { size: Short(Four), repr: Some(Float16) }),
            "mat4x2f16" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Two }, repr: Some(Float16) }),
            "mat4x3f16" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Three }, repr: Some(Float16) }),
            "mat4x4f16" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Four }, repr: Some(Float16) }),

            "mat2f32" => Self::Mat(MatrixTy { size: Short(Two), repr: Some(Float32) }),
            "mat2x2f32" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Two }, repr: Some(Float32) }),
            "mat2x3f32" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Three }, repr: Some(Float32) }),
            "mat2x4f32" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Four }, repr: Some(Float32) }),
            "mat3f32" => Self::Mat(MatrixTy { size: Short(Three), repr: Some(Float32) }),
            "mat3x2f32" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Two }, repr: Some(Float32) }),
            "mat3x3f32" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Three }, repr: Some(Float32) }),
            "mat3x4f32" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Four }, repr: Some(Float32) }),
            "mat4f32" => Self::Mat(MatrixTy { size: Short(Four), repr: Some(Float32) }),
            "mat4x2f32" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Two }, repr: Some(Float32) }),
            "mat4x3f32" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Three }, repr: Some(Float32) }),
            "mat4x4f32" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Four }, repr: Some(Float32) }),

            "mat2f64" => Self::Mat(MatrixTy { size: Short(Two), repr: Some(Float64) }),
            "mat2x2f64" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Two }, repr: Some(Float64) }),
            "mat2x3f64" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Three }, repr: Some(Float64) }),
            "mat2x4f64" => Self::Mat(MatrixTy { size: Long { columns: Two, rows: Four }, repr: Some(Float64) }),
            "mat3f64" => Self::Mat(MatrixTy { size: Short(Three), repr: Some(Float64) }),
            "mat3x2f64" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Two }, repr: Some(Float64) }),
            "mat3x3f64" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Three }, repr: Some(Float64) }),
            "mat3x4f64" => Self::Mat(MatrixTy { size: Long { columns: Three, rows: Four }, repr: Some(Float64) }),
            "mat4f64" => Self::Mat(MatrixTy { size: Short(Four), repr: Some(Float64) }),
            "mat4x2f64" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Two }, repr: Some(Float64) }),
            "mat4x3f64" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Three }, repr: Some(Float64) }),
            "mat4x4f64" => Self::Mat(MatrixTy { size: Long { columns: Four, rows: Four }, repr: Some(Float64) }),

            "frag" => Self::Frag,
            "vert" => Self::Vert,
            "pos" => Self::Pos,
            "cos" => Self::Cos,
            "sin" => Self::Sin,
            "tan" => Self::Tan,
            "cosh" => Self::Cosh,
            "sinh" => Self::Sinh,
            "tanh" => Self::Tanh,
            "acos" => Self::Acos,
            "asin" => Self::Asin,
            "atan" => Self::Atan,
            "atan2" => Self::Atan2,

            _ => {
                // if value.starts_with("vec") && value.len() <= 7 {
                //     eprintln!("Can't parse vector typenames yet! (given \"{value}\")");
                //     todo!("parse vector typenames");
                // }
                // if value.starts_with("mat") && value.len() <= 9 {
                //     eprintln!("Can't parse matrix typenames yet! (given \"{value}\")");
                //     todo!("parse matrix typenames");
                // };
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

use yuri_common::{DimCount, FloatBits, ScalarTy};

use crate::item::Attribute;
use crate::{Ident, MatrixDim, Qpath, VectorRepr};

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct MatrixTy {
    pub size: MatrixDim,
    pub repr: Option<FloatBits>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq)]
pub struct VectorTy {
    pub size: DimCount,
    pub repr: VectorRepr,
}

// FUTURE: replace u32 with partial knowledge (current Yuri spec only has unknown/known)
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum ArrayLength {
    Runtime,
    FixedSize(u32),
}

#[derive(Debug, Clone, PartialEq)]
pub struct ArrayTy {
    pub length: ArrayLength,
    pub element_ty: Box<WrittenTy>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundTy {
    pub fields: Vec<CompoundTyField>,
}

#[derive(Clone, Debug, PartialEq)]
pub struct CompoundTyField {
    // pub parent: Ywk<CompoundType<'a>>,
    pub attributes: Vec<Attribute>,
    pub name: Ident,
    pub field_ty: WrittenTy,
}

#[derive(Clone, Debug, PartialEq)]
pub enum WrittenTy {
    Alias(Qpath),
    Bool,
    Scalar(ScalarTy),
    Vector(VectorTy),
    Matrix(MatrixTy),
    Array(ArrayTy),
    Compound(CompoundTy),
    // TODO
    Enum,
}

pub const WRITTEN_MAT4F: WrittenTy = WrittenTy::Matrix(MatrixTy {
    size: MatrixDim::Short(DimCount::Four),
    repr: None,
});
pub const WRITTEN_VEC2F: WrittenTy = WrittenTy::Vector(VectorTy {
    size: DimCount::Two,
    repr: VectorRepr::Float(None),
});
pub const WRITTEN_VEC3F: WrittenTy = WrittenTy::Vector(VectorTy {
    size: DimCount::Three,
    repr: VectorRepr::Float(None),
});
pub const WRITTEN_VEC4F: WrittenTy = WrittenTy::Vector(VectorTy {
    size: DimCount::Four,
    repr: VectorRepr::Float(None),
});

macro_rules! written_from_helper {
    ($from:ty, $variant:ident) => {
        impl From<$from> for WrittenTy {
            fn from(value: $from) -> Self {
                WrittenTy::$variant(value)
            }
        }

        impl From<$from> for Box<WrittenTy> {
            fn from(value: $from) -> Self {
                Box::new(WrittenTy::$variant(value))
            }
        }
    };
}
written_from_helper!(ScalarTy, Scalar);
written_from_helper!(VectorTy, Vector);
written_from_helper!(MatrixTy, Matrix);
written_from_helper!(ArrayTy, Array);
written_from_helper!(CompoundTy, Compound);

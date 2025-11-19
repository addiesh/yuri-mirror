use yuri_common::{DimensionCount, FloatBits, ScalarTy};

use crate::item::Attribute;
use crate::{Ident, Qpath};

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct MatrixTy {
    pub columns: DimensionCount,
    pub rows: DimensionCount,
    pub bitsize: Option<FloatBits>,
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct VectorTy {
    pub size: DimensionCount,
    pub repr: ScalarTy,
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
    pub element_ty: WrittenTy,
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

#[derive(Debug, Clone, PartialEq)]
pub enum WrittenTy {
    Alias(Qpath),
    Bool,
    Scalar(ScalarTy),
    Vector(VectorTy),
    Matrix(MatrixTy),
    Array(Box<ArrayTy>),
    Compound(Box<CompoundTy>),
}

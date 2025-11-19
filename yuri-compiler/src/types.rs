use yuri_common::{DimensionCount, FloatBits, IntBits, ScalarTy};

use crate::Ywk;
use crate::attribute::Attribute;
use crate::error::{ResolutionError, TypeError};
use crate::item::TypeAliasItem;
use crate::resolution::Resolution;
use crate::types::array::ArrayType;
use crate::types::compound::{CompoundType, CompoundTypeField};
use crate::types::primitive::Primitive;

pub mod array;
pub mod compound;
pub mod primitive;

pub trait Typeable<'a>: Sized + 'a {
    /// Attempt to fit `self` into `other` (`self` must be a subtype of `other`). This function is not guaranteed to be commutative.
    /// # Intersection Rules
    /// - `(Prim(Some(a)), Prim(Some(b))) if a == b => Prim(Some(a))`
    ///   - When A and B are 100% equal, no conversions are required.
    /// - `(Prim(Some(a)), Prim(None)) => Prim(Some(a))`
    ///   - A is a subtype of B, and can be used in place of B.
    /// - `(Prim(None), Prim(Some(_))) => Err`
    ///   - B is a subtype of A, which is invalid.
    ///   - If the target (`other`) is `Bool(Some(false))`, then an unknown boolean may not fulfill this type relationship.
    /// - `(Prim(Some(a)), Prim(Some(b))) if a != b => Err`
    ///   - When A and B are known to not be equal, this is a simple type mismatch.
    /// - `(Unreachable, _) => Unreachable`
    ///   - Unreachable may "act" as any value.
    ///   - In the following Rust code, the `if` expression will broaden (union) to i32 unless it is KNOWN to pick a branch that produces Unreachable (Yuri can determine this).
    ///   ```rust
    ///   let x: i32 = if condition {
    ///       return 5;
    ///   } else {
    ///       4
    ///   };
    ///   ```
    /// - `(_, Unreachable) => Err`
    ///   - If `Unreachable` is expected, anything that ISN'T unreachable is invalid.
    ///   - In the following Rust code, the block expression will evaluate to `Never`.
    ///   ```rust
    ///   let x: Never = {
    ///     return 5;
    ///   };
    ///   let y: i32 = 0;
    ///   ```
    /// - `(_, _) => Err`
    ///   - Two completely unrelated types cannot fit into each other because
    ///     Yuri has no runtime types, sum types, or inheritance.
    fn intersect_with(&self, other: &Self) -> Result<Self, TypeError<'a>>;

    /// Attempt to find the most specific supertype of both `self` and `other`. This function must be commutative.
    /// # Union Rules
    /// - `(a, b) if a == b => a`
    ///   - When A and B are 100% equal, no conversions are required.
    ///   - This includes primitive `None` pairs, `Unreachable` pairs, and any other odd types that are fully-equal.
    /// - `(Prim(Some(_)), Prim(None)) | (Prim(None), Prim(Some(_))) => Prim(None)`
    ///   - If either A or B is a subtype of the other, pick the supertype.
    /// - `(Prim(Some(a)), Prim(Some(b))) if a != b => Prim(None)`
    ///   - If A or B are known to be not equal but have a common supertype, use that.
    /// - `(Unreachable, x) | (x, Unreachable) => x`
    ///   - Unreachable types cannot exist, so they are essentially removed from the equation leaving only the other type provided.
    /// - `(_, _) => Err`
    ///   - Two completely unrelated types cannot fit into each other.
    fn union(this: &Self, other: &Self) -> Result<Self, TypeError<'a>>;

    fn is_resolved(&self) -> bool;
}

/// To the Yuri compiler, all "values" are actually types.
/// Whenver you would store a *value* in the Yuri AST, you store a type.
/// Whenever you would store a *type annotation* in the Yuri AST, you store a type.
#[derive(Clone, Debug, PartialEq)]
pub enum TypeValue<'src> {
    Primitive(Primitive),
    Array(Box<ArrayType<'src>>),
    Compound(Box<CompoundType<'src>>),
    /// Resolves to another type
    Alias(Resolution<Ywk<TypeAliasItem<'src>>>),
    /// A value that cannot exist. Anything after an expression with this type will be ignored.
    Unreachable,
}

impl<'src> From<&yuri_parser::types::WrittenTy> for TypeValue<'src> {
    fn from(value: &yuri_parser::types::WrittenTy) -> Self {
        use yuri_parser::types::WrittenTy;
        match value {
            WrittenTy::Bool => TypeValue::Primitive(Primitive::Bool(None)),
            WrittenTy::Scalar(scalar_ty) => todo!(),
            WrittenTy::Vector(vector_ty) => todo!(),
            WrittenTy::Matrix(matrix_ty) => todo!(),
            WrittenTy::Array(array_ty) => todo!(),
            WrittenTy::Compound(compound_ty) => TypeValue::Compound(Box::new(CompoundType {
                fields: compound_ty
                    .fields
                    .iter()
                    .map(|field| CompoundTypeField {
                        name: field.name,
                        attributes: field
                            .attributes
                            .iter()
                            .map(|attrib| Attribute {
                                path: Resolution::Unresolved(attrib.path.clone()),
                                _todo_params: Default::default(),
                            })
                            .collect(),
                        field_type: Into::into(&field.field_ty),
                    })
                    .collect(),
            })),
            WrittenTy::Alias(qpath) => TypeValue::Alias(Resolution::Unresolved(qpath.clone())),
        }
    }
}

impl<'src> Typeable<'src> for TypeValue<'src> {
    fn is_resolved(&self) -> bool {
        match self {
            TypeValue::Primitive(_) | TypeValue::Unreachable => true,
            TypeValue::Array(array) => array.is_resolved(),
            TypeValue::Compound(compound_type) => compound_type.is_resolved(),
            // TODO: this part irks me
            TypeValue::Alias(Resolution::Resolved { item_path: _, item }) => {
                item.upgrade().is_some()
            }
            TypeValue::Alias(Resolution::Unresolved(_)) => false,
        }
    }

    fn intersect_with(&self, other: &Self) -> Result<Self, TypeError<'src>> {
        use TypeValue::*;
        match (self, other) {
            (Primitive(a), Primitive(b)) => a.intersect_with(b).map(Primitive),
            (Array(a), Array(b)) => a.intersect_with(b).map(Box::new).map(Array),
            (Compound(a), Compound(b)) => a.intersect_with(b).map(Box::new).map(Compound),
            (Unreachable, Unreachable) => Ok(TypeValue::Unreachable),

            // unresolved alias
            (Alias(Resolution::Unresolved(l0)), Alias(Resolution::Unresolved(r0))) => {
                Err(TypeError::Multiple(vec![
                    TypeError::Unresolved(ResolutionError(l0.clone())),
                    TypeError::Unresolved(ResolutionError(r0.clone())),
                ]))
            }

            (Alias(Resolution::Unresolved(item_path)), _)
            | (_, Alias(Resolution::Unresolved(item_path))) => {
                Err(TypeError::Unresolved(ResolutionError(item_path.clone())))
            }

            (Alias(Resolution::Resolved { item_path, item }), other) => {
                if let Some(this) = item.upgrade() {
                    this.lock().unwrap().aliases.intersect_with(other)
                } else {
                    Err(TypeError::Unresolved(ResolutionError(item_path.clone())))
                }
            }

            (this, Alias(Resolution::Resolved { item_path, item })) => {
                if let Some(upgraded) = item.upgrade() {
                    this.intersect_with(&upgraded.lock().unwrap().aliases)
                } else {
                    Err(TypeError::Unresolved(ResolutionError(item_path.clone())))
                }
            }

            _ => Err(TypeError::UnrelatedType(self.clone(), other.clone())),
        }
    }

    fn union(this: &Self, other: &Self) -> Result<Self, TypeError<'src>> {
        use crate::types;
        use TypeValue::*;
        match (this, other) {
            (Primitive(a), Primitive(b)) => types::Primitive::union(a, b).map(Primitive),
            (Array(a), Array(b)) => ArrayType::union(a, b).map(Box::new).map(Array),
            (Compound(a), Compound(b)) => CompoundType::union(a, b).map(Box::new).map(Compound),
            (Unreachable, x) | (x, Unreachable) => Ok(x.clone()),

            // This shortcut works because unions are commutative.
            (Alias(alias), x) | (x, Alias(alias)) => {
                let aliases = alias.try_upgrade()?;
                let aliases = aliases.lock().unwrap();
                let aliases = &aliases.aliases;

                let result = TypeValue::union(x, aliases);
                // validate commutativity
                #[cfg(debug_assertions)]
                {
                    let check = TypeValue::union(aliases, x);
                    match (&result, &check) {
                        (Ok(result), Ok(check)) => debug_assert_eq!(result, check),
                        // at the very least, it commutatively errors
                        (Err(_result), Err(_check)) => {}
                        _ => panic!(
                            "The commutativity guarantees of `TypeValue::union` were not upheld with type {this:?} and {other:?} (caused during alias upgrading)"
                        ),
                    }
                }
                result
            }

            _ => Err(TypeError::UnrelatedType(this.clone(), other.clone())),
        }
    }
}

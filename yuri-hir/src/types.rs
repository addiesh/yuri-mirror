use crate::Ywk;
use crate::error::TypeError;
use crate::item::{FunctionItem, TypeAliasItem};
use crate::resolution::Resolution;
use crate::types::array::ArrayTyVal;
use crate::types::compound::CompoundTyVal;
use crate::types::primitive::PrimitiveTyVal;

pub mod array;
pub mod compound;
pub mod enumerate;
pub mod primitive;

pub trait Typeable: Sized {
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
    fn intersect_with(&self, other: &Self) -> Result<Self, TypeError>;

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
    fn union(this: &Self, other: &Self) -> Result<Self, TypeError>;

    fn is_resolved(&self) -> bool;
}

// gave up on generics and traits for types because of how much jank it would create.
// Sure, I could represent the type system within another type system, but would it be convenient?
// Absolutely not.

/// Implemented by all TYPE STRUCTS.
// trait IsTy {}
/// Implemented by all VALUE STRUCTS.
// trait ValueOfTy<T: IsTy + ?Sized> {}

#[derive(Clone, Debug)]
pub enum TyVal {
    Primitive(PrimitiveTyVal),
    Array(Box<ArrayTyVal>),
    Compound(Box<CompoundTyVal>),
    /// Resolves to another type
    Alias(Resolution<Ywk<TypeAliasItem>>),
    /// A function type.
    /// Importantly, Yuri has no way (yet) to express higher-order functions.
    Function(Resolution<Ywk<FunctionItem>>),
    /// A value that cannot exist.
    Unreachable,
}

use crate::types::TyVal;

#[derive(Clone, Debug)]
pub struct ArrayAnnotation {
    /// If this is Some, we know all elements of the array must be this type.
    /// If this is None, we don't know what the type of each element is.
    pub element_type: Box<TyVal>,

    /// If this is Some, we know the size of the array at compile-time.
    /// If this is None, the array is unsized.
    pub len: Option<u32>,
}

#[derive(Clone, Debug)]
pub struct ArrayTyVal {
    // TODO: we can't really represent a homogenous array easily when the type of the array element is only known at runtime.
    // like, the size is known at runtime, but we can't do much with that.
    // the best we could do with type shamanry here is a generic on TypeValue (which would suck!)
    // vecs of boxed types suck.
    /// If this is Some, this can be a VALUE or a TYPE.
    /// If `explicit_type.len` is also Some, the number of elements must be equal to the `explicit_type.len`.
    /// If this is None, this can only be a TYPE.
    pub elements: Option<Vec<TyVal>>,

    pub explicit_type: Option<ArrayAnnotation>,
}

impl ArrayTyVal {
    pub fn per_element_type(&self) -> Option<TyVal> {
        todo!()
    }

    #[rustfmt::skip]
    pub fn length(&self) -> Option<Option<u32>> {
        match self {
            // invalid
            ArrayTyVal { elements: None, explicit_type: None } => None,
            // values only
            ArrayTyVal { elements: Some(elements), explicit_type: None } => Some(Some(elements.len() as u32)),
            // annotation only
            ArrayTyVal { elements: None, explicit_type: Some(explicit_type) } => Some(explicit_type.len),
            // values and annotation
            ArrayTyVal { elements: Some(elements), explicit_type: Some(explicit_type) } => match (elements.len() as u32, explicit_type.len) {
                // sized array can be coerced to unsized
                (l1, None) => Some(Some(l1)),
                // matching fixed lengths
                (l1, Some(l2)) if l1 == l2 => Some(Some(l1)),
                // inequal lengths
                _ => None
            }
        }
    }

    /// Whether this array is "valid," ensuring homogeny and matching type annotations.
    pub fn is_valid(&self) -> bool {
        // TODO: this is so complicated...
        todo!("validity checking for arrays is unimplemented")
    }
}

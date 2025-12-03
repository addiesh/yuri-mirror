use crate::{expression::Expression, resolution::Resolution};

/// Provides a "compiler plugin" that works on attribute semantics.
pub trait AttributeProvider: Send + Sync + Unpin + 'static {
    // TODO: implement methods
    fn process_hl(&self) -> ();
    fn process_ll(&self) -> ();
}

#[derive(Clone, Debug)]
pub struct Attribute {
    // attributes will be weird.
    pub path: Resolution<()>,
    pub params: Vec<Expression>,
}

impl PartialEq for Attribute {
    fn eq(&self, other: &Self) -> bool {
        // TODO: I don't like this.
        let a = &self.path;
        let b = &other.path;
        match (a, b) {
            (Resolution::Unresolved(l0), Resolution::Unresolved(r0))
            | (Resolution::Unresolved(l0), Resolution::Resolved { item_path: r0, .. })
            | (Resolution::Resolved { item_path: l0, .. }, Resolution::Unresolved(r0))
            | (
                Resolution::Resolved { item_path: l0, .. },
                Resolution::Resolved { item_path: r0, .. },
            ) => l0 == r0,
        }
    }
}

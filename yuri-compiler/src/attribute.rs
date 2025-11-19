use crate::resolution::Resolution;

/// Provides a "compiler plugin" that works on attribute semantics.
pub trait AttributeProvider<'a>: 'a + Send + Sync + Unpin {
    // TODO: implement methods
    fn process_hl(&self) -> ();
    fn process_ll(&self) -> ();
}

#[derive(Clone, Debug)]
pub struct Attribute<'a> {
    // attributes will be weird.
    pub path: Resolution<&'a dyn AttributeProvider<'a>>,
    pub _todo_params: Option<Vec<()>>,
}

impl<'a> PartialEq for Attribute<'a> {
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

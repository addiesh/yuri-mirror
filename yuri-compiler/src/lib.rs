use std::sync::Mutex;

use crate::item::{FunctionItem, TypeAliasItem, VariableItem};
use crate::types::TypeValue;

pub mod attribute;
pub mod error;
pub mod expression;
pub mod item;
pub mod lower;
pub mod resolution;
pub mod scope;
#[cfg(test)]
mod test;
pub mod types;

// these types exist so we can change thread-safety easily later
pub type Yrc<T> = std::sync::Arc<Mutex<T>>;
pub type Ywk<T> = std::sync::Weak<Mutex<T>>;

// #[derive(Debug, Clone, Copy, PartialEq)]
// pub enum Ident {
//     Id(usize),
//     // SyntheticId(usize),
//     Keyword(Keyword),
// }

#[inline]
fn split_results<T, E>(iterator: impl Iterator<Item = Result<T, E>>) -> (Vec<T>, Vec<E>) {
    let (elements, errors): (Vec<_>, Vec<_>) = iterator.partition(Result::is_err);

    (
        elements
            .into_iter()
            .filter_map(Result::ok)
            .collect::<Vec<_>>(),
        errors
            .into_iter()
            .filter_map(Result::err)
            .collect::<Vec<_>>(),
    )
}

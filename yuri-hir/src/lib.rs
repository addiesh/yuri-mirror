use std::sync::Mutex;

use crate::item::{FunctionItem, TypeAliasItem, VariableItem};

pub mod attribute;
pub mod error;
pub mod expression;
pub mod item;
pub mod resolution;
pub mod scope;
pub mod types;

// these types exist so we can change thread-safety easily later
pub type Yrc<T> = std::sync::Arc<Mutex<T>>;
pub type Ywk<T> = std::sync::Weak<Mutex<T>>;

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

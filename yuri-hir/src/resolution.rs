use yuri_ast::Qpath;

use crate::error::ResolutionError;
use crate::{Yrc, Ywk};

#[derive(Clone)]
pub enum Resolution<T> {
    Unresolved(Qpath),
    Resolved { item_path: Qpath, item: T },
}

impl<T> Resolution<T> {
    pub fn try_inline(&self) -> Result<T, ResolutionError>
    where
        T: Clone,
    {
        match self {
            Resolution::Unresolved(item_path) => Err(ResolutionError(item_path.clone())),
            Resolution::Resolved { item, .. } => Ok(item.clone()),
        }
    }
}

impl<T> Resolution<Ywk<T>> {
    pub fn try_upgrade(&self) -> Result<Yrc<T>, ResolutionError> {
        match self {
            Resolution::Unresolved(item_path) => Err(ResolutionError(item_path.clone())),
            Resolution::Resolved { item_path, item } => {
                if let Some(arc) = item.upgrade() {
                    Ok(arc)
                } else {
                    Err(ResolutionError(item_path.clone()))
                }
            }
        }
    }
}

impl<T> PartialEq for Resolution<T> {
    // TODO: i don't like this
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (
                Resolution::Resolved { item_path: l0, .. },
                Resolution::Resolved { item_path: r0, .. },
            ) => l0 == r0,
            _ => false,
        }
    }
}

// impl<'a, T: PartialEq> PartialEq for Resolution<'a, T> {
//     fn eq(&self, other: &Self) -> bool {
//         match (self, other) {
//             (Resolution::Resolved { item: l0, .. }, Resolution::Resolved { item: r0, .. }) => {
//                 l0 == r0
//             }
//             _ => false,
//         }
//     }
// }

impl<T> std::fmt::Debug for Resolution<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Unresolved(identifier) => f.debug_tuple("Unresolved").field(identifier).finish(),
            Self::Resolved {
                item_path: identifier,
                item: _item,
            } => f.debug_tuple("Resolved").field(identifier).finish(),
        }
    }
}

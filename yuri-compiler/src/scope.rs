use yuri_parser::Qpath;

use crate::item::{FunctionItem, TypeAliasItem, VariableItem};
use crate::{Yrc, Ywk};

/// A "rule" in yuri is a guarantee that some condition will always
/// be true wherever the rule is in place. This allows for, among other things,
/// more advanced optimization. As such, this is a complicated idea to implement,
/// so we *probably* won't be implementing it for the V1 Yuri compiler.
pub struct ScopeRule {/* empty for now */}

pub enum ScopeItem {
    Variable(Yrc<VariableItem>),
    Parameter(Yrc<VariableItem>),
    Function(Yrc<FunctionItem>),
    Import(Yrc<Qpath>),
    TypeAlias(Yrc<TypeAliasItem>),
}

#[derive(Default)]
pub struct Scope {
    /// - For root modules, this is None.
    /// - For function blocks, this is the block/module scope in which they are defined.
    /// - For expression blocks (including if/else/loop blocks), this is either the function scope
    ///   or the parent block scope (recursive) in which it is contained.
    pub parent: Option<Ywk<Scope>>,
    pub items: Vec<ScopeItem>,
    /// Currently unused. In the future, this may be used to create assertions on values and types
    /// based on checked predicates.
    pub _rules: Vec<ScopeRule>,
}

impl Scope {
    /// An empty scope; the [Default] implementation.
    #[inline]
    pub fn new() -> Self {
        Default::default()
    }
}

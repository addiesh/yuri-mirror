use std::error::Error;
use std::fmt::{Debug, Display};

use yuri_hir::error::{ResolutionError, TypeError};
use yuri_parser::error::ParseError;

// TODO: improve error reporting
#[derive(Debug)]
pub enum CompileError<'src> {
    Multiple(Vec<CompileError<'src>>),
    Parse(ParseError),
    Unresolved(ResolutionError),
    NotMyFault(Box<dyn Error + Send + Sync + 'src>),
    TypeCheck(Box<TypeError>),
}

impl From<Box<dyn Error + Send + Sync>> for CompileError<'_> {
    fn from(value: Box<dyn Error + Send + Sync>) -> Self {
        Self::NotMyFault(value)
    }
}

impl<'a> From<ParseError> for CompileError<'a> {
    fn from(value: ParseError) -> Self {
        CompileError::Parse(value)
    }
}

impl<'a> Clone for CompileError<'a> {
    fn clone(&self) -> Self {
        match self {
            Self::Multiple(arg0) => Self::Multiple(arg0.clone()),
            Self::Parse(arg0) => Self::Parse(arg0.clone()),
            Self::NotMyFault(arg0) => {
                Self::NotMyFault(format!("(CLONED MESSAGE): {arg0:?}").into())
            }
            Self::TypeCheck(arg0) => Self::TypeCheck(arg0.clone()),
            Self::Unresolved(arg0) => Self::Unresolved(arg0.clone()),
        }
    }
}
impl<'a> Display for CompileError<'a> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        // just use debug
        Debug::fmt(&self, f)
    }
}

impl<'a> Error for CompileError<'a> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        None
    }
}

impl<'src> From<ResolutionError> for CompileError<'src> {
    fn from(value: ResolutionError) -> Self {
        Self::Unresolved(value)
    }
}

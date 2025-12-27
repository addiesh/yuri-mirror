use yuri_ast::Qpath;

use crate::expression::Expression;

#[derive(Clone, Debug)]
pub struct Attribute {
    // attributes will be weird.
    pub path: Qpath,
    pub args: Vec<Expression>,
}

use std::{borrow::Cow, error::Error, fs::File};

use nalgebra::{Matrix2, Matrix3, Matrix4, Vector2, Vector3, Vector4};
use tree_sitter::Parser;

mod codegen;

type YuriError = Box<dyn Error>;

/// Represents one "compilation context" of Yuri.
pub struct YuriContext<'a> {
    documents: Vec<YuriDocument<'a>>,
}

/// Represents a series of modules, functions, global variables, and type aliases/definitions.
pub struct YuriDocument<'a> {
    submodules: Vec<()>,
    functions: Vec<YuriFunction<'a>>,
    globals: Vec<()>,
    type_aliases: Vec<()>,
}

pub trait YuriAttributeSource {}

// Currently, the Yuri type system can only discriminate between **Known** and **Unknown** values.
// In future versions, the goal is to implement algebraic solving for partial unknowns.
pub enum YuriPrimitive {
    Bool(Option<bool>),

    Float1(Option<f32>),
    Float2(Option<Vector2<f32>>),
    Float3(Option<Vector3<f32>>),
    Float4(Option<Vector4<f32>>),

    Signed1(Option<i32>),
    Signed2(Option<Vector2<i32>>),
    Signed3(Option<Vector3<i32>>),
    Signed4(Option<Vector4<i32>>),

    Unsigned1(Option<u32>),
    Unsigned2(Option<Vector2<u32>>),
    Unsigned3(Option<Vector3<u32>>),
    Unsigned4(Option<Vector4<u32>>),

    Mat2(Option<Matrix2<f32>>),
    Mat3(Option<Matrix3<f32>>),
    Mat4(Option<Matrix4<f32>>),
}

pub struct YuriAttribute {
    // attributes will be weird.
    name: String,
}

pub enum YuriType<'a> {
    Primitive(YuriPrimitive),
    Compound(YuriCompoundType<'a>),
}

pub struct YuriValueItem<'a> {
    attributes: Vec<YuriAttribute>,
    name: Cow<'a, str>,
    value_type: YuriType<'a>,
}

pub struct YuriCompoundType<'a> {
    fields: Box<[YuriValueItem<'a>]>,
}

pub struct YuriFunction<'a> {
    attributes: Vec<YuriAttribute>,
    name: Cow<'a, str>,
    parameters: Vec<YuriValueItem<'a>>,
    return_type: YuriType<'a>,
}

pub fn _print_syntax_tree(code: &str) -> Result<(), YuriError> {
    let mut parser = Parser::new();
    parser.set_language(&tree_sitter_yuri::LANGUAGE.into())?;
    let tree = parser.parse(code, None).unwrap();
    let tree_file = File::create("syntax_tree_dot")?;
    tree.print_dot_graph(&tree_file);
    let root_node = tree.root_node();

    println!("{root_node}");
    Ok(())
}

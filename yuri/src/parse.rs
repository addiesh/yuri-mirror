// use log::{debug, warn};
// use tree_sitter::{Parser, TreeCursor};

// use crate::{
//     YuriError,
//     compile::{FunctionItem, Module, NamedItem, ScopeItems, Symbol, TypeAlias, TypeValue},
// };

// fn parse_type<'a>(code: &str, cursor: &mut TreeCursor) -> Result<TypeValue<'a>, YuriError> {
//     let node = cursor.node();
//     use crate::compile::Primitive::*;
//     match node.grammar_name() {
//         "primitive_type" => Ok(TypeValue::Primitive(match &code[node.byte_range()] {
//             "f" => Float1(None),
//             "f2" => Float2(None),
//             "f3" => Float3(None),
//             "f4" => Float4(None),
//             "u" => Unsigned1(None),
//             "u2" => Unsigned2(None),
//             "u3" => Unsigned3(None),
//             "u4" => Unsigned4(None),
//             "i" => Signed1(None),
//             "i2" => Signed2(None),
//             "i3" => Signed3(None),
//             "i4" => Signed4(None),
//             "mat2" => Mat2(None),
//             "mat3" => Mat3(None),
//             "mat4" => Mat4(None),
//             "bool" => Bool(None),
//             _ => unreachable!(),
//         })),
//         #[expect(unused)]
//         "identifier" => Ok(TypeValue::Alias(Box::new(TypeAlias {
//             aliases: None,
//             name: {
//                 // node.child(i)
//                 todo!()
//             },
//         }))),
//         "compound_type_item" => todo!(),
//         "array_type_item" => todo!(),
//         _ => unreachable!(),
//     }
// }

// fn parse_function<'a>(code: &str, cursor: &mut TreeCursor) -> Result<FunctionItem<'a>, YuriError> {
//     // let attributes;
//     // let name;
//     // let parameters;
//     // let return_type;
//     // let body;

//     let node = cursor.node();
//     let node_name = node.grammar_name();
//     cursor.goto_first_child();
//     let function_name = &code[cursor.node().byte_range()];
//     debug!("parsing function (name = {function_name:?})");
//     cursor.goto_next_sibling();
//     let return_type = parse_type(code, cursor)?;
//     debug!("node: {:?}", node.grammar_name());

//     match node_name {
//         "line_comment" => { /* skip */ }

//         _ => {
//             return Err(format!(
//                 "Unknown node type {node_name:?} in function declaration (this is a compiler bug!)"
//             )
//             .into());
//         }
//     }

//     // if !cursor.goto_next_sibling() {
//     //     break;
//     // }

//     #[expect(unreachable_code)]
//     Ok(FunctionItem {
//         return_type,
//         attributes: todo!(),
//         name: todo!(),
//         parameters: todo!(),
//         body: todo!(),
//     })
// }

// #[expect(unused)]
// fn parse_module_node<'a>(
//     name: String,
//     code: &str,
//     cursor: &mut TreeCursor,
// ) -> Result<Module<'a>, YuriError> {
//     let mut submodules = Vec::<Module<'a>>::new();
//     let mut functions = Vec::<FunctionItem<'a>>::new();
//     let mut variables = Vec::<NamedItem<'a>>::new();
//     let mut type_aliases = Vec::<TypeAlias<'a>>::new();
//     let mut imports = Vec::<Symbol<'a>>::new();
//     let mut errors = Vec::<YuriError>::new();

//     loop {
//         let node = cursor.node();
//         let node_type = node.grammar_name();
//         debug!("node: {:?}", node.grammar_name());

//         match node_type {
//             "ERROR" => { /* add to error stack */ }
//             "line_comment" => { /* skip */ }
//             "function_item" => match parse_function(code, cursor) {
//                 Ok(function) => functions.push(function),
//                 Err(err) => errors.push(err),
//             },
//             "type_alias_item" => {}
//             "module_item" => {
//                 // recursion!
//             }
//             "global_item" => {}

//             _ => {
//                 return Err(
//                     format!("Unknown node type {node_type:?} in module declaration (this is a compiler bug!)").into(),
//                 );
//             }
//         }

//         if !cursor.goto_next_sibling() {
//             break;
//         }
//     }

//     Ok(Module {
//         name,
//         submodules,
//         scope: ScopeItems {
//             functions,
//             variables,
//             type_aliases,
//             imports,
//         },
//     })
// }

use crate::{YuriError, compile::Module};

pub(super) fn parse_module<'a>(name: String, code: &str) -> Result<Module<'a>, YuriError> {
    todo!()
    //     let mut parser = Parser::new();

    //     parser.set_language(&tree_sitter_yuri::LANGUAGE.into())?;
    //     let tree = parser.parse(code, None).unwrap();
    //     let root_node = tree.root_node();
    //     let mut cursor = root_node.walk();

    //     // root_node.child_count == 0
    //     if !cursor.goto_first_child() {
    //         warn!("parsed empty module");
    //         Ok(Module::empty(name))
    //     } else {
    //         parse_module_node(name, code, &mut cursor)
    //     }
}

mod duck {
    use crate::*;

    #[test]
    fn compound_types() {
        // let pairs = [
        //     (
        //         CompoundType { fields: vec![] },
        //         CompoundType { fields: vec![] },
        //         Ok(CompoundType { fields: vec![] }),
        //     ),
        //     (
        //         CompoundType {
        //             fields: vec![CompoundTypeField {
        //                 name: "single type".into(),
        //                 attributes: vec![],
        //                 field_type: TypeValue::Primitive(Primitive::Bool(None)),
        //             }],
        //         },
        //         CompoundType { fields: vec![] },
        //         Err(CompileError::IncompatibleFieldCount(1, 0)),
        //     ),
        // ];

        // for (a, b, expected) in pairs {
        //     assert_eq!(a.quack(&b), expected);
        // }
    }
}

#[test]
fn artificial_module() {
    // let mut example_root_scope = Arc::new(Scope {
    //     parent: None,
    //     variables: vec![],
    //     parameters: vec![],
    //     functions: vec![],
    //     imports: vec![],
    //     type_aliases: vec![],
    //     rules: vec![],
    // });

    // let mut function_scope = Arc::new(Scope {
    //     parent: Some(Arc::downgrade(&example_root_scope)),
    //     variables: vec![],
    //     parameters: vec![],
    //     functions: vec![],
    //     imports: vec![],
    //     type_aliases: vec![],
    //     rules: vec![],
    // });

    // let uv_param = Arc::new(VariableItem {
    //     name: "uv".into(),
    //     parent_scope: Arc::downgrade(&function_scope),
    //     explicit_type: Some(TypeValue::Primitive(Primitive::Float2(None))),
    //     value: None,
    // });

    // function_scope.parameters.push(uv_param);

    // let variable_let = Arc::new(VariableItem {
    //     name: "variable".into(),
    //     parent_scope: Arc::downgrade(&function_scope),
    //     explicit_type: None,
    //     value: Some(Expression::Literal(ExpressionLiteral::Integer(0))),
    // });

    // let arrrr_let = Arc::new(VariableItem {
    //     name: "arrrr".into(),
    //     parent_scope: Arc::downgrade(&function_scope),
    //     explicit_type: Some(TypeValue::Array(Array {
    //         elements: None,
    //         explicit_type: Some(ArrayType {
    //             element_type: Box::new(TypeValue::Primitive(Primitive::Unsigned1(None))),
    //             len: Some(8),
    //         }),
    //     })),
    //     value: Some(Expression::Array(ArrayExpression::Spread {
    //         element: Box::new(Expression::Variable(Resolution::Resolved {
    //             identifier: ItemPath::new(&["variable"]).unwrap(),
    //             item: Arc::downgrade(&variable_let),
    //         })),
    //         length: Box::new(Expression::Literal(ExpressionLiteral::Integer(8))),
    //     })),
    // });

    // Arc::new(FunctionItem {
    //     attributes: vec![],
    //     name: "test".into(),
    //     return_type: TypeValue::Primitive(Primitive::Mat2(None)),
    //     body: BlockExpression {
    //         scope: function_scope,
    //         statements: vec![BlockStatement::Variable(Arc::downgrade(
    //             &function_scope.variables[1],
    //         ))],
    //         tail: None,
    //     },
    //     parameters: vec![uv_param],
    // });

    // let not_std_scope = Arc::new_cyclic(|scope| Scope {
    //     parent: Some(Arc::downgrade(&example_root_scope)),
    //     variables: vec![],
    //     parameters: vec![],
    //     functions: vec![Arc::new(FunctionItem {
    //         attributes: vec![],
    //         name: "test".into(),
    //         parameters: vec![],
    //         return_type: TypeValue::Primitive(Primitive::Mat2(None)),
    //         body: BlockExpression {
    //             scope: Arc::new(Scope {
    //                 parent: Some(scope.clone()),
    //                 variables: vec![],
    //                 parameters: vec![],
    //                 functions: vec![],
    //                 imports: vec![],
    //                 type_aliases: vec![],
    //                 rules: vec![],
    //             }),
    //             statements: vec![],
    //             tail: None,
    //         },
    //     })],
    //     imports: vec![],
    //     type_aliases: vec![],
    //     rules: vec![],
    // });

    // let example = Arc::new_cyclic(|scope| Module {
    //     parent: None,
    //     name: "example".into(),
    //     submodules: vec![Arc::new(Module {
    //         parent: Some(scope.clone()),
    //         name: "not_std".into(),
    //         submodules: vec![],
    //         scope: not_std_scope,
    //     })],
    //     scope: example_root_scope,
    // });
}

#[test]
fn _understanding_rustc() {
    let ur = {
        return;
    };

    println!("unreachable")
}

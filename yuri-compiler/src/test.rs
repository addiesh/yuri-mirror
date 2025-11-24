#[test]
fn test_lower() {
    use yuri_ast::InStorage;

    let source = "
# test
@frag
fn test(uv: f2): mat2 {

	let variable = 0

	let arrrr: [u, 8] = [variable; 8]

	if false {
		return 1
	} else if false {
		2
	} else if false {
		3
	} else {
		4
	}
}
";

    let tokens: Box<[_]> = yuri_lexer::tokenize(source).collect();
    let mut storage = InStorage::default();

    let (ast, errors) = yuri_parser::parse_all(source, &mut storage, &tokens);
    if !errors.is_empty() {
        panic!("Parser returned errors: {errors:?}");
    }

    let name = storage.to_ident("test_module");

    let _lowered = crate::lower::lower(source, &mut storage, &ast, name).unwrap();
}

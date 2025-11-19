use thin_vec::thin_vec;

#[test]
fn test_lower() {
    use yuri_parser::ParseStorage;

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
    let mut storage = ParseStorage::default();

    let ast = yuri_parser::parse_all(&mut storage, source, &tokens).unwrap();

    let lowered = crate::lower::lower(&mut storage, source, &ast).unwrap();
}

use crate::{ParseStorage, parse::ParseState};

#[test]
fn im_himejoshing_out_rn() {
    let source = "x == y";
    let tokens: Box<[_]> = yuri_lexer::tokenize(source).collect();
    let mut storage = ParseStorage::default();

    let mut state = ParseState {
        storage: &mut storage,
        source,
        tokens: &tokens,
        index: 0,
        byte_offset: 0,
        errors: Vec::new(),
    };

    for tok in &tokens {
        println!(" - {tok:?}");
    }

    let thing = state.expr_compare().unwrap();
    println!("{thing:?}");
}

#[test]
fn im_fujoing_out_rn() {
    let source = "1 + 2 * 3 / 4 == 5 or 6 != +~7";
    let tokens: Box<[_]> = yuri_lexer::tokenize(source).collect();
    let mut storage = ParseStorage::default();

    let mut state = ParseState {
        storage: &mut storage,
        source,
        tokens: &tokens,
        index: 0,
        byte_offset: 0,
        errors: Vec::new(),
    };

    for tok in &tokens {
        println!(" - {tok:?}");
    }

    let thing = state.expression().unwrap();
    println!("{thing:?}");
}

use crate::to_snake_case;

#[test]
fn test_to_snake_case() {
    let tests = [
        ("a_b_c", "a! b! c"),
        ("abc", "abc"),
        ("____abc", "_abc"),
        ("____ab__c", "_ab_c"),
        ("_abc", "_abc"),
    ];
    for test in tests {
        assert_eq!(test.0, to_snake_case(test.1))
    }
}

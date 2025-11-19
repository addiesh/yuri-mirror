# The Yuri Shader Compiler

[![crates](https://img.shields.io/crates/v/yuri-lang.svg)](https://crates.io/crates/yuri-lang)

The [Yuri](https://yuri-lang.dev) shader compiler.

## Outline of Compilation Steps

1. Lexer: text -> tokens
    - token: struct { enum + span }
2. Parser: tokens -> AST
3. HL Compiler (lowering): AST -> behavior tree
    - resolve identifiers to absolute paths ("collection" in rustc)
    - check semantics and types usage
    - (last) invoke attributes on behavior tree
4. HL Optimizer: behavior tree -> behavior tree
    - compile-time evaluation goes here
5. IR Compiler: behavior tree -> IR mod
    - apply attribute functions on IR
6. IR Optimizer: IR mod -> IR mod
7. Linker: IR mod -> bytecode


## Notes

- We intend to create C-family bindings for the compiler API, but this is not a priority.

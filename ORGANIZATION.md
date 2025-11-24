# Structure of the Yuri Compiler

1. The caller (CLI/game engine/other host) reads a source code file into a buffer.
1. The *lexer* groups those characters into a linear array of _tokens_ (such as `Ident`, `OpenParen`, `Star`, etc.).
1. The tokens are then turned into a tree-based data  structure (the _AST_) by the *parser.* Name resolution also happens here.
1. The syntax tree is *lowered* into an _intermediate representation_ (IR).
Here, we wait for other source files to reach this point, merge them together, then validate their semantics for correctness.
At this point, Yuri's implementation is up in the air, but another level of intermediate representation may be used before
finally beginning *code generation,* where any remaining IR is turned into code for the target platform.
For Yuri, that means generating SPIR-V bytecode for Vulkan.

# Common

- Provide a common representation for data shared between all stages of the compiler

# The Lexer

 - Converts text -> tokens

Current problems:
- [ ] Numeric literals are improperly categorized

# The Parser

- Converts tokens -> AST
- Interns strings and identifiers
- Performs name resolution

Important Types
- `Ident`: An interned identifier.
- `Qpath`: A sequence of `Ident`s.
- `InStorage`: Interns strings.

Current problems:
- [ ] Numeric literals are improperly categorized
- [ ] Fix error handling/recovery
- [ ] Certain expressions can't be parsed
- [ ] Declarations/statements can't be parsed

# The IR Compiler

- Converts AST -> High-Level IR
- Performs type-checking on all expressions and functions
- Performs semantic validation

# (working)

- Generate layout information

# The Codegen Backend

- Converts High-Level IR to SPIR-V bytecode

Current problems:
- Doesn't exist

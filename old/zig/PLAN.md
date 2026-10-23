# To write the lexer

The general idea is to reuse the `tokenizer_example.zig`, for MiniCC. We will write a `tokenizer.zig` for MiniCC in exactly the same style. So, in order to do that:

- Identify tokens handled in the C++ code (`lexer.cc`), and make a list. 

- Also identify the C++ reserved words handled in `lexer.cc`, and make a list.

- Read `tokenizer_example.zig` and see how tokens and reserved words are handled (what data structures and enums), since we want to use the same style exactly.

- Write the `tokenizer.zig` file using `tokenizer_example.zig` as a template, but adapting every part to our case (that is, our list of tokens and our list of reserved words). Leave everything else the same (the `Token` struct, the `Loc` struct, the `State` enum, etc.). Zig and C++ are similar at this level, so it is just a matter of changing what tokens are handled.




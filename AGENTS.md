# AI Agent Instructions for MiniCC

This project is an interpreter for a C++ subset for educational purposes. It tries to simplify all aspects of C++ so that it is easier to grasp and to fully know for novice programmers.

## Folders

- The `ideas` folder is just a bunch of documents with ideas, ignore it.
- The `src` folder contains the source of the whole project.
- The `tests` folder contains all the tests. These are separated into further folders that can be invoked independently by calling the `test.sh` script.

## Compiling

The `Makefile` is the entry point for everything: compile, test, clean and format. 

Possible commands are:

```bash
make          # Debug build
make release  # Release build
make test     # Build + pass test suite
make clean    # Clean intermediate files
```

## Description of each file

- `ast.{cc,hh}`: The abstract syntax tree representation.

- `astpr.{cc,hh}`: The AST printer, i.e., a function that will dump any AST and show all its details. This is exactly the format used in the `tests/ast` folder.

- `commands.hh`: Header file concerned with commands issued on the command line. That is, "subcommands" in the style `minicc tok`, `minicc ast`, etc. Line 144 of `main.cc` lists the available commands that are there.

- `error.hh`: Various types of errors used within the project.

- `i18n.{cc,hh}`: Very basic and stupid form of internationalization.

- `interpreter.{cc,hh}`: An old and very slow interpreter that uses the AST directly. I want to replace this with a VM.

- `lexer.{cc,hh}`: The lexer.

- `main.cc`: The main file which invokes the different commands.

- `parser.cc`: The parser, which produces the AST from the source code, using the `Lexer` class from `lexer.hh`.

- `pos.hh`: A class representing a position in the source code, to remember where each part of the code was.

- `pprint.{hh,cc}`: The pretty-printer, that is, takes an AST and produces the source code that represents that AST, in a default style (it is not configurable).

- `semantic.{cc,hh}`: The semantic analyzer, which is very basic and probably wrong in many ways.

- `stepper.{cc,hh}`: The stepper, which is humongous since it also works over the AST, just as the interpreter. A very bad idea.

- `table.hh`: an association table made with `vector` instead of map, in data-oriented fashion. Very much used in `types.*` and `vm.*`.

- `test.{cc.hh}`: test functions, which invoke tests, compare outputs, etc.

- `token.{cc,hh}`: an `enum` with all possible tokens and some functions to deal with them.

- `types.{cc,hh}`: Type system for execution time (used by the interpreter). Gone wild, it should only exist at compilation time, probably.

- `value.{cc,hh}`: A dynamic value class, also used by the interpreter. Bad ideas again.

- `vm.{cc,hh}`: The beginnings of a VM for the language, but incomplete.

- `walker.{cc,hh}`: A generic AST walker, which is only used right now for collecting errors but which was planned to be used for any other process walking the AST (`astpr`, `pprint`, ...).





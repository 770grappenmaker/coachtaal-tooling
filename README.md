# coachtaal-tooling
Small monorepo that contains a lexer, parser, interpreter and compiler (to JVM bytecode / .class files) for the CMA Coach 7 language.
It currently supports most standard library functions, supports English and Dutch, and supports a few procedures and statements.
This project is intended to simplify and provide QOL features for Coach 7 modeling (which is a mandatory part of the Dutch Physics exam curriculum)
by providing proper error logs and a programmable interface.

### Speed
This implementation is, even though there are little to none optimizations used, significantly faster than the official Coach software.
The interpreter is about 50-200x faster (depending on the model and the amount of iterations), and the compiler (excluding compile times) is 500-1500x faster
(these figures are rough estimates based on measurements on my local computer, and should be taken with a grain of salt, but the order of magnitude should match).

### Features
*An item with a ticked checkbox is already implemented, otherwise, the item is still yet to be implemented*.
- [x] Lexing the programs
- [x] Building a tree structure (for evaluation and analysis)
- [x] Validating the code
- [x] Multiple languages (Dutch and English)
- [x] Logging data (and converting to TSV/CSV)
- [x] Visually view the data (`visualizer`)
- [x] Compile to JVM bytecode (and dynamically loading and executing)
- [ ] Do..while, repeat
- [ ] Function and procedure definition and usage
- [ ] LSP / IntelliJ plugin

### Project Setup
1. Clone this repository
2. Run `git submodule update --init --recursive`
3. Run `./gradlew build` to build an artifact for every submodule

### License
The [Unlicense](LICENSE.md).
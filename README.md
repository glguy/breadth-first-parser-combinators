# Breadth-First Parser Combinators
This package explores a parser-combinator design that allows the input stream to be released as early as possible. This is similar to the ReadP parser combinator module except ReadP uses backtracking to implement *biased choice*.
## Generalized primitives
This package also explores primitives beyond functions on input tokens. This can be interesting because the parser state is represented as a data type and if the primitives are also data types then the state can be exhaustively explored or analyzed.
## Building
To build Haskell libraries you'll need GHC and cabal-install: https://www.haskell.org/ghcup/
```
$ git clone https://github.com/glguy/breath-first-parser-combinators
$ cd breath-first-parser-combinators
$ cabal build
```
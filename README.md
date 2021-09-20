# autocomplete-haskell

Haskell implementation of autocomplete with functions for working with the [trie](https://en.wikipedia.org/wiki/Trie#:~:text=In%20computer%20science%2C%20a%20trie,key%2C%20but%20by%20individual%20characters.) data structure. This repository also contains a [C interface](c_library) to the underlying Haskell implementation.

`autocomplete-haskell` was developed as my final project for [36-750: Statistical Computing](https://36-750.github.io/) at Carnegie Mellon University.

Some key files and directories:
- [src/Tries.hs](src/Tries.hs): The primary tries and autocomplete implementations.
- [test/Spec.hs](test/Spec.hs): Unit tests.
- [benchmark/Bench.hs](benchmark/Bench.hs): Benchmarks.
- [Data](Data): Example strings with weights for performing autocomplete.
- [app/Main.hs](app/Main.hs): Implementation of simple command line interface.

## Dependencies

The instructions which follow will rely on [stack](https://docs.haskellstack.org/en/stable/README/).

## Perform autocomplete from the command line

For this example, we will select the top 3 matches for the prefix "Char" in the database [Data/pokemon.txt](Data/pokemon.txt).

```zsh
stack install

~/.local/bin/autocomplete-haskell-exe Data/pokemon.txt "Char" 3
```

Try it out with other data files, prefixes, and values of k!

## Run unit tests

Haskell unit tests can be found in [`test/Spec.hs`](test/Spec.hs) and can be run using `stack`:

```zsh 
stack test
```

## Benchmarks

Benchmarks for running autocomplete on various datasets in [Data](Data) can be found in [`benchmark/Bench.hs`](benchmark/Bench.hs`). These can be run using `stack`:

```zsh 
stack bench
```

## Documentation (using Haddock)

Documentation for autocomplete-haskell can be generated using [haddock](https://www.haskell.org/haddock/) by running the following:

```zsh
stack haddock --open autocomplete-haskell
```

## C interface

This repository also contains a C foreign function interface ([FFI](https://wiki.haskell.org/Foreign_Function_Interface)) to the main functions implemented in [Tries.hs]. For more details, see [c_library](c_library).

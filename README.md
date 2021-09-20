# autocomplete-haskell

This repository contains a Haskell implementation of autocomplete with functions for working with the [trie](https://en.wikipedia.org/wiki/Trie) data structure, as well as a [C foreign function interface](c_library) to the underlying Haskell implementation.

`autocomplete-haskell` was developed as my final project for [36-750: Statistical Computing](https://36-750.github.io/) at Carnegie Mellon University.

Some important files and folders:
- [src/Tries.hs](src/Tries.hs): The primary tries and autocomplete implementations.
- [test/Spec.hs](test/Spec.hs): Unit tests.
- [benchmark/Bench.hs](benchmark/Bench.hs): Benchmarks.
- [Data](Data): Example datasets containing strings with weights for performing autocomplete.
- [app/Main.hs](app/Main.hs): Implementation of simple command line interface.

## Dependencies

The instructions which follow will rely on [stack](https://docs.haskellstack.org/en/stable/README/). This can likely be installed using your OS package manager. For example, using Homebrew, 

```zsh
brew install haskell-stack
```

## Perform autocomplete from the command line

The file [app/Main.hs](app/Main.hs) implements a command line interface to the `autocomplete` function in [src/Tries.hs](src/Tries.hs). An executable is available after running `stack install`:

For this example, we will select the top 3 matches for the prefix "Char" in the database [Data/pokemon.txt](Data/pokemon.txt).

```zsh
stack install

~/.local/bin/autocomplete-haskell-exe Data/pokemon.txt "Char" 3
```

Expected output:
```zsh
[(147161.0,"Charizard"),(390.0,"Charmeleon"),(306.0,"Charmander")]
```

Try it out with other data files, prefixes, and values of k!

## Run unit tests

Haskell unit tests are found in [`test/Spec.hs`](test/Spec.hs) and can be run using `stack`:

```zsh 
stack test
```

## Benchmarks

Benchmarks for running autocomplete on various datasets in [Data](Data) are found in [`benchmark/Bench.hs`](benchmark/Bench.hs). These can be run using `stack`:

```zsh 
stack bench
```

## Documentation (using Haddock)

Documentation for autocomplete-haskell can be generated using [haddock](https://www.haskell.org/haddock/) by running the following:

```zsh
stack haddock --open autocomplete-haskell
```

## C foreign function interface (FFI)

This repository also contains a C [FFI](https://wiki.haskell.org/Foreign_Function_Interface) to the main functions implemented in [src/Tries.hs](src/Tries.hs). For more details, see [c_library](c_library).

## Acknowledgements

Thanks to [Alex Reinhart](https://www.refsmmat.com/) ([capnrefsmmat](https://github.com/capnrefsmmat)) for many helpful suggestions on improving code/documentation/test quality. 

# autocomplete-haskell

## Dependencies

- [stack](https://docs.haskellstack.org/en/stable/README/)

## Run unit tests

Haskell unit tests can be found in [`test/Spec.hs`](test/Spec.hs) and can be run using `stack`:

```zsh 
stack test
```

## Benchmarks

Benchmarks for running autocomplete on various datasets in [./Data](./Data) can be found in [`bench/Bench.hs`](bench/Bench.hs`). These can be run using `stack`:

```zsh 
stack bench
```

## Haddock documentation

Documentation for autocomplete-haskell can be generated using [haddock](https://www.haskell.org/haddock/) by running the following:

```zsh
stack haddock --open autocomplete-haskell
```

## C interface

See [c_library/][./c_library/]

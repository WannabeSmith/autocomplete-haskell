#!/bin/bash

# Go to stack project directory
cd ..

# compile the Haskell library
stack build

# Compile the to-be-exported FFI functions and place them in a
# dependencies folder in the c_library
stack ghc -- -c -O src/CTries.hs -stubdir c_library/dependencies

# Compile the C functions and create examples
stack ghc -- --make -no-hs-main -optc-O -Ic_library/dependencies -Ic_library/src c_library/examples/trie_examples.c c_library/src/tries.c src/CTries -o c_library/examples/trie_examples

# run examples
c_library/examples/trie_examples


#!/bin/bash

# Go to stack project directory
cd ..

# compile the Haskell library
stack build

# Compile the to-be-exported FFI functions and place them in a
# dependencies folder in the c_library
stack ghc -- -c -O src/CTries.hs -stubdir c_library/dependencies

# Compile and link C functions with CTries functions 
stack ghc -- --make -no-hs-main -optc-O -Ic_library/dependencies c_library/src/tries.c src/CTries -c 


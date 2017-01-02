#!/bin/bash

# http://stackoverflow.com/questions/32123475/profiling-builds-with-stack
stack build --executable-profiling --library-profiling --ghc-options "-O -fprof-auto -rtsopts" &&
    stack exec -- hs-bt-tracker-bin +RTS -N -p

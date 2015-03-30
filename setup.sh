#!/bin/sh
cabal update
cabal clean
cabal sandbox init


cabal install -j --only-dependencies --enable-profiling


cabal configure --enable-profiling
cabal build -j
echo "Running with profiling..."
dist/build/ModelTest/ModelTest


cabal configure --disable-profiling
cabal build -j
echo "Running without profiling..."
dist/build/ModelTest/ModelTest
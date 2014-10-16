#!/bin/sh

CABAL=${CABAL:='cabal'}

git submodule init
git submodule update
$CABAL sandbox init
$CABAL sandbox add-source sigym4-geometry
$CABAL install --only-dependencies  $@
$CABAL build

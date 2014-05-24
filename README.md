hs-cudd: Haskell bindings to the CU Decision Diagram library
============================================================

[![Build Status](https://secure.travis-ci.org/bradlarsen/hs-cudd.svg)](http://travis-ci.org/bradlarsen/hs-cudd)

This package provides Haskell bindings to CUDD, the
[CU Decision Diagram library](http://vlsi.colorado.edu/~fabio/CUDD/), a C
library implementing several decision diagram data structures, such as
binary decision diagrams and algebraic decision diagrams.

These bindings include a copy of CUDD 2.5.0, and should be entirely
self-contained: you don't need to install CUDD elsewhere on your system.

Currently, these bindings only provide access to a subset of the binary
decision diagram functionality.

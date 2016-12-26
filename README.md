# Range-tree
---

[![Build Status](https://travis-ci.org/Martoon-00/range-tree.svg?branch=master)](https://travis-ci.org/Martoon-00/range-tree)

Multidimentional range-tree with fractional cascading.

Homework for "Advanced data structures" course.

### Implementation

Classical range-tree with fractional cascading :)


### Tests

Tests are based on `QuickCheck` library, which allows us to run our implementation on automatically generated test input samples, and so compare it with naive solution.

We have tests for different number dimensions, with different type of coordinates: `Double`, `Small Int` and `Bool`. Last two help to check case when input contains equal points.


### Benchamarks

Becnhmarks are written with help of `criterion` library. Once taking measurements, it provides `html` report with histograms.

We have benchmarks for `build` and `find` functions, each cover different dimensions and different sizes of input point set.

You can consider scripts in [bench](https://github.com/Martoon-00/range-tree/tree/master/bench) folder to launch benchmarks; same folder contains sample benchmark results (`.html` files).

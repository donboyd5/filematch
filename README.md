
<!-- README.md is generated from README.Rmd. Please edit that file -->

# filematch

<!-- badges: start -->
<!-- badges: end -->

filematch has tools that aid in constrained statistical matching of two
microdata files that have record weights.

These data files commonly are called file A and file B. Typically they
have a set of X variables in common. In addition, A has a set of Y
variables and B has a set of Z variables. The goal is to create an AB
file that has X, Y, and Z variables.

An unconstrained match does not necessarily ensure that… A constrained
match ensures that …

An excellent package that has many tools for unconstrained file matching
is `StatMatch`.

This package has a different focus that StatMatch: its goal is to ensure
that the weighted file AB has weighted sums for the Y variables that
match the weighted sums of Y in A, and also has weighted sums of Z
variables that match

## Installation

You can install the development version of filematch from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("donboyd5/filematch")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(filematch)
## basic example code
```

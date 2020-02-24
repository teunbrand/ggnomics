
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ggnomics <img src="man/figures/logo_300px.png" align = "right" width = "150" />

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![Travis build
status](https://travis-ci.org/teunbrand/ggnomics.svg?branch=master)](https://travis-ci.org/teunbrand/ggnomics)
[![Codecov test
coverage](https://codecov.io/gh/teunbrand/ggnomics/branch/BioC/graph/badge.svg)](https://codecov.io/gh/teunbrand/ggnomics?branch=BioC)
<!-- badges: end -->

This branch of ggnomics explores how to integrate Bioconductor S4 class
constructions and ggplot2 plotting system. Ideally, this would be made
near seamless at some point. Practically, it is still a buggy mess.

## Installation

You can install this experimental branch from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("teunbrand/ggnomics", ref = "BioC")
```

It would probably only work with the current release candidate of
ggplot2, which you can install as such:

``` r
# install.packages("devtools")
devtools::install_github("tidyverse/ggplot2", ref = "v3.3.0-rc")
```

## Example

There are no examples yet. It is still under construction. Will post
more once something might actually be useful.

## Likely advantages

Here is a list:

  - Grammar of graphics approach to plotting data
  - Converting a DataFrame to be ggplot compatible will retain S4
    classes
  - Support for genomic scales
  - Can be extended
  - Layer data will remain S4 throughout the build process
      - Which means geoms/stats/positions have access to the class

## Known bugs

Here is a list:

  - Cannot execute evaluation of aesthetics when the referred column is
    S4
  - Using GRanges as position unit is slow
  - There is no way (yet) to represent non-integers on genomic scales.
  - Doesn’t have a polar coordinate system
  - No working implementation of discrete scales
  - No working List-like solution yet (e.g. IntegerList etc.)
  - Mostly geared toward IRanges/GRanges and numerical-Rle classes at
    this point, whether anything else will work is anyone’s guess
  - No supported scale transformations so far
  - If you even breathe too loudly in the general direction of your
    screen, it will throw a hissyfit

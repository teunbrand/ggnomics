
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
devtools::install_github("tidyverse/ggplot2", ref = "master")
```

## Example

The main idea is that you would be able to plot S4 Vector classes as you
would any other vector.

``` r
suppressPackageStartupMessages({
  library(ggnomics)
  library(GenomicRanges)
})

df <- DataFrame(
  x = GRanges(c("chr1:100-200:+", "chr1:150-250:-", "chr2:200-300:*")),
  y = 1:3
)

g <- ggplot(df, aes(x, y, fill)) +
  geom_tile(width = 0, height = 0.8)
g
```

<img src="man/figures/README-genomic_scale-1.png" width="60%" style="display: block; margin: auto;" />

The data retains the S4 classes throughout the vast majority of plot
building, allowing specialised geoms and stats to take advantage of
this.

``` r
layer_data(g)
#>                x y PANEL group           xmin           xmax ymin ymax   fill
#> 1 chr1:100-200:+ 1     1    -1 chr1:100-200:+ chr1:100-200:+  0.6  1.4 grey20
#> 2 chr1:150-250:- 2     1    -1 chr1:150-250:- chr1:150-250:-  1.6  2.4 grey20
#> 3 chr2:200-300:* 3     1    -1 chr2:200-300:* chr2:200-300:*  2.6  3.4 grey20
#>   colour size linetype alpha width height
#> 1     NA  0.1        1    NA     0    0.8
#> 2     NA  0.1        1    NA     0    0.8
#> 3     NA  0.1        1    NA     0    0.8
```

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

  - Cannot evaluate expressions on S4 columns in aesthetics ([work in
    progress]())
  - Using GRanges as position unit is slow
  - Currently cannot represent non-integers on genomic scales.
  - Doesn’t have a polar coordinate system
  - No working implementation of discrete scales
  - No working List-like solution yet (e.g. IntegerList etc.)
  - Mostly geared toward IRanges/GRanges and numerical-Rle classes at
    this point, whether anything else will work is anyone’s guess
  - No supported scale transformations so far
  - If you even breathe too loudly in the general direction of your
    screen, it will throw a hissyfit

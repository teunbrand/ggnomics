
<!-- README.md is generated from README.Rmd. Please edit that file -->

# Note

This branch of the repository exists, such that a standalone version of facet_nested() is preserved for people who would like to use  that piece of code without downloading the entire package.

# ggnomics <img src="man/figures/icon.png" align = "right" width = "120" />

This package was written to extend `ggplot2` in the following way:

1.  Introduce flexibility in facetting and colour scaling.
2.  Provide some functionality for plotting genomics data.

Towards the first goal, this package provides the function
`force_panelsize()` to force ggplot panels to adopt a specific ratio or
absolute dimension. It also provides `scale_fill/colour_multi()` to map
multiple aesthetics to different colour scales. To make the handling of
subsets of data as seperate geoms easier, it provides `ggsubset()`.

Towards the second goal it provides functions such as `facet_ideogrid()`
and `facet_ideowrap()` which will put ideograms with highlights next to
facetted panels. Also the new Hi-C triangle geom (`geom_hictriangle`)
makes it easy to display Hi-C matrices from the
[GENOVA](https://github.com/robinweide/GENOVA) package in a 45° rotated
fashion.

Please keep in mind that this package is highly experimental, may be
unstable and is subject to changes in the future.

## Installation

You can install ggnomics from github with:

``` r
# install.packages("devtools")
devtools::install_github("teunbrand/ggnomics")
```

## Example

This is a basic example of `scale_colour_multi()`, `facet_nested()`,
`force_panelsizes()` and `ggsubset()` in action:

``` r
library(ggplot2)
library(ggnomics)

phi <- 2/(1 + sqrt(5))

df <- iris
df$nester <- ifelse(df$Species == "setosa",
                    "Short Leaves",
                    "Long Leaves")

g <- ggplot(df, aes(x = Sepal.Length, y = Petal.Length)) +
  geom_point(aes(sepal.width = Sepal.Width),
             ggsubset(Species == "setosa")) +
  geom_point(aes(petal.length = Petal.Length),
             ggsubset(Species == "versicolor")) +
  geom_point(aes(petal.width = Petal.Width),
             ggsubset(Species == "virginica")) +
  facet_nested(~ nester + Species, scales = "free") +
  scale_colour_multi(aesthetics = c("sepal.width", 
                                    "petal.length", 
                                    "petal.width"),
                     colours = list(c("white", "green"),
                                    c("white", "red"),
                                    c("white", "blue")),
                     guide = guide_colourbar(barheight = unit(50, "pt"))) +
  force_panelsizes(rows = 1, cols = c(1, phi, phi^2), respect = TRUE)
#> Warning: Ignoring unknown aesthetics: sepal.width
#> Warning: Ignoring unknown aesthetics: petal.length
#> Warning: Ignoring unknown aesthetics: petal.width
g
```

![](man/figures/README-example-1.png)<!-- -->

The warning about ignoring unknown aesthetics can be circumvented by
updating the defaults of geoms,
e.g.:

``` r
ggplot2::update_geom_defaults("point", list(fill = c("sepal.width", "petal.length", "petal.width")))
```

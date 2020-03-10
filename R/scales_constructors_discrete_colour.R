# Minimal docs in scales_constructors_continuous_colour.R

# Hue palette -------------------------------------------------------------

#' @describeIn S4_colour_scales See \code{\link[ggplot2]{scale_colour_hue}}
#' @export
#' @usage NULL
scale_colour_S4_hue <- function(
    ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
    direction = 1, na.value = "grey50", aesthetics = "colour"
) {
    S4_discrete_scale(aesthetics, "hue",
                      scales::hue_pal(h, c, l, h.start, direction),
                      na.value = na.value, ...)
}

#' @describeIn S4_colour_scales See \code{\link[ggplot2]{scale_colour_hue}}
#' @export
#' @usage NULL
scale_fill_S4_hue <- function(
    ..., h = c(0, 360) + 15, c = 100, l = 65, h.start = 0,
    direction = 1, na.value = "grey50", aesthetics = "fill"
) {
    S4_discrete_scale(aesthetics, "hue",
                      scales::hue_pal(h, c, l, h.start, direction),
                      na.value = na.value, ...)
}


# Default scales ----------------------------------------------------------

#' @describeIn S4_colour_scales See \code{\link[ggplot2]{scale_colour_discrete}}
#' @export
#' @usage NULL
scale_colour_S4_discrete <- scale_colour_S4_hue

#' @describeIn S4_colour_scales See \code{\link[ggplot2]{scale_colour_discrete}}
#' @export
#' @usage NULL
scale_fill_S4_discrete <- scale_fill_S4_hue

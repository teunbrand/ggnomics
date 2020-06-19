# Documentation -----------------------------------------------------------

#' @name S4_colour_scales
#' @title S4 colour scales
#'
#' @description This collection of functions are analogues of ggplot2's
#'   colour/fill scales but use the S4 compatible scale system under the hood.
#'   Please refer to the ggplot2 documentation.
NULL

# Default scales ----------------------------------------------------------

# Basically, the default scales exisits such that scale_type has a reference for
# what the scale should be in lieu of a user defined scale

#' @describeIn S4_colour_scales See
#'   \code{\link[ggplot2]{scale_colour_continuous}}.
#' @export
#' @usage NULL
scale_colour_S4_continuous <- function(
    ...,
    type = getOption("ggplot2.continuous.colour", default = "gradient")
) {
    switch(
        type,
        gradient = scale_colour_S4_gradient(...),
        viridis = scale_colour_S4_viridis_c(...),
        rlang::abort("Unknown scale type")
    )
}

#' @describeIn S4_colour_scales See
#'   \code{\link[ggplot2]{scale_colour_continuous}}.
#' @export
#' @usage NULL
scale_fill_S4_continuous <- function(
    ...,
    type = getOption("ggplot2.continuous.fill", default = "gradient")) {
    switch(
        type,
        gradient = scale_fill_S4_gradient(...),
        viridis = scale_fill_S4_viridis_c(...),
        rlang::abort("Unknown scale type")
    )
}

# Gradients ---------------------------------------------------------------

# Simple ports, just switches from using continuous_scale() to
# S4_continuous_scale()

#' @describeIn S4_colour_scales See
#'   \code{\link[ggplot2]{scale_colour_gradient}}.
#' @export
#' @usage NULL
scale_colour_S4_gradient <- function(
    ..., low = "#132B43", high = "#56B1F7", space = "Lab",
    na.value = "grey50", guide = "colourbar", aesthetics = "colour"
) {
    S4_continuous_scale(aesthetics, "gradient",
                        scales::seq_gradient_pal(low, high, space),
                        na.value = na.value, guide = guide, ...)
}

#' @describeIn S4_colour_scales See
#'   \code{\link[ggplot2]{scale_colour_gradient}}.
#' @export
#' @usage NULL
scale_fill_S4_gradient <- function(
    ..., low = "#132B43", high = "#56B1F7", space = "Lab",
    na.value = "grey50", guide = "colourbar", aesthetics = "fill"
) {
    S4_continuous_scale(aesthetics, "gradient",
                        scales::seq_gradient_pal(low, high, space),
                        na.value = na.value, guide = guide, ...)
}


# Viridis -----------------------------------------------------------------

#' @describeIn S4_colour_scales See
#'   \code{\link[ggplot2]{scale_colour_viridis_c}}.
#' @export
#' @usage NULL
scale_colour_S4_viridis_c <- function(
    ..., alpha = 1, begin = 0, end = 1,
    direction = 1, option = "D", values = NULL,
    space = "Lab", na.value = "grey50",
    guide = "colourbar", aesthetics = "colour"
) {
    S4_continuous_scale(
        aesthetics,
        "viridis_c",
        scales::gradient_n_pal(
            scales::viridis_pal(alpha, begin, end, direction, option)(6),
            values,
            space
        ),
        na.value = na.value,
        guide = guide,
        ...
    )
}

#' @describeIn S4_colour_scales See
#'   \code{\link[ggplot2]{scale_colour_viridis_c}}.
#' @export
#' @usage NULL
scale_fill_S4_viridis_c <- function(
    ..., alpha = 1, begin = 0, end = 1,
    direction = 1, option = "D", values = NULL,
    space = "Lab", na.value = "grey50",
    guide = "colourbar", aesthetics = "fill"
) {
    S4_continuous_scale(
        aesthetics,
        "viridis_c",
        scales::gradient_n_pal(
            scales::viridis_pal(alpha, begin, end, direction, option)(6),
            values,
            space
        ),
        na.value = na.value,
        guide = guide,
        ...
    )
}

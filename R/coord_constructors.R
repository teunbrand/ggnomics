#' @title Cartesian coordinates for S4 classes
#'
#' @description The Cartesian coordinate system is the typical linear coordinate
#'   system wherein a point on the plane is parameterised by the x- and
#'   y-position and distances between point are Euclidean. The S4 variant allows
#'   the contextual evaluation of, for example, ranged data classes.
#'
#' @inheritParams ggplot2::coord_cartesian
#'
#' @details This coordinate function is designed to work with S4 scales. In
#'   absence thereof, no contextual evaluation should occur.
#'
#'   As a temporary workaround, when the coordinates are set up, all S4 Vector
#'   classes are disguised as the \code{WoodenHorse} class.
#'
#' @return A \code{Coord} ggproto object.
#'
#' @export
#'
#' @examples
#' NULL
coord_S4 <- function(xlim = NULL, ylim = NULL, expand = TRUE, default = FALSE,
                     clip = "on") {
  ggproto(NULL, CoordS4, limits = list(x = xlim, y = ylim),
          expand = expand, default = default, clip = clip)
}

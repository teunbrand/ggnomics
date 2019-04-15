#' Hi-C Triangle
#'
#' The Hi-C triangle geom is used to portray a rotated Hi-C matrix such that the
#' diagonal runs along the x-axis. It can be convenient for displaying details
#' located in a region of interest and proximal to the diagonal.
#'
#' @param exp A GENOVA experiment
#' @param ranges A GRanges object. Alternatively, a \code{list} of at least
#'   length 3 with the following elements: \describe{\item{\code{character}}{
#'   vector of chromosome names}\item{\code{integer}}{ vector of start
#'   positions}\item{\code{integer}}{ vector of end positions}}
#' @param stat,position,...,na.rm,show.legend,inherit See
#'   \code{\link[ggplot2]{geom_polygon}}
#'
#' @details The triangle is calculated such that it is rotated by 45 degrees
#'   clockwise and rescaled such that the x-axis is equivalent to the original
#'   region and the y-axis corresponds to distance from the diagonal.
#'   \code{geom_hictriangle} invokes these steps at the layer level by
#'   specifying how a raster-like Hi-C matrix should be setup as a triangle.
#'
#'   A recommendation for this geom is to use a \code{coord_fixed(ratio = 0.5)}
#'   to get neat 45 degree angles.
#'
#' @export
#'
#' @seealso \code{\link[GENOVA]{construct.experiment}} on how to construct a
#'   GENOVA experiment object. \code{\link[GenomicRanges]{GRanges}} on how to
#'   make or subset GRanges objects.
geom_hictriangle <- function(exp, ranges, stat = "identity", position = "identity", ...,
                             na.rm = FALSE, show.legend = NA)
{
  if(!check_valid_hiclayer(exp1 = exp, exp2 = NULL, xranges = ranges, yranges = NULL)){
    stop("Invalid Hi-C Layer")
  }
  data <- extract_hicdata(exp1, exp2 = NULL, xranges = ranges, yranges = NULL, triangle = TRUE)
  rm(exp, ranges)
  mapping <- aes(x = x, y = y, fill = contacts)
  layer(data = data, mapping = mapping, stat = stat, geom = GeomHicTriangle,
        position = position, show.legend = show.legend, inherit.aes = FALSE,
        params = list(na.rm = na.rm, ...))
}

triangle_setup <- function(data, params){

  # Assign groups and estimate resolution
  data$group <- 1:nrow(data)
  res <- resolution(data$x)/2

  # Calculate coordinates
  xmin <- data$x - res
  xmax <- data$x + res
  ymin <- data$y - res
  ymax <- data$y + res
  coords <- cbind(c(xmin, xmin, xmax, xmax),
                  c(ymin, ymax, ymax, ymin))

  # Rotate and scale coordinates
  rotmat <- matrix(c(0.5, -1, 0.5, 1), ncol = 2)
  newcoords <- t(rotmat %*% (t(coords)))

  # New data
  data <- data.frame(x = newcoords[,1], y = newcoords[,2],
                     fill  = rep(data$fill, 4),
                     PANEL = rep(data$PANEL, 4),
                     group = rep(data$group, 4))
  data <- data[order(data$group),]
  data <- data[data$y > -1,]

  data
}

#' @rdname geom_hictriangle
#' @importFrom ggplot2 ggproto GeomPolygon
GeomHicTriangle <- ggplot2::ggproto("GeomHicTriangle", ggplot2::GeomPolygon,
                           setup_data = triangle_setup)

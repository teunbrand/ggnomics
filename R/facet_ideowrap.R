#' Wrap a 1d ribbon of panels into 2d with ideograms
#'
#' \code{facet_ideowrap()} mimicks behaviour of
#' \code{\link[ggplot2]{facet_wrap}} but adds ideograms. The facetting variables
#' are checked for chromosome names and if these are found, then ideograms are
#' plotted in between the strips and panels.
#'
#' @inheritParams ggplot2::facet_wrap
#' @param ideo.size \code{\link[grid]{unit}} value object of class \code{"unit"}
#'   specifying ideogram size.
#' @param high.col \code{NA} (default) or colour value of ideogram highlights
#'   (see details). If \code{NA}, no highlighting will occur.
#'
#' @details In absence of chromosome names in variables will result in default
#'   \code{facet_grid()} behaviour. A mix of chromosome names and other
#'   variables results in whitespace between the panels and other variable
#'   strips.
#'
#'   By default, this function will not highlight the panel axis range. To
#'   highlight the plotted region on the ideogram, set \code{high.col} to a
#'   colour. If highlights occur, region to be highlighted will be computed from the
#'   relevant axis limits of the corresponding panels.
#'
#'   Colours can be specified in one of the forms returned by
#'   \code{\link[grDevices]{rgb}}, as name (see \code{\link[grDevices]{colors}})
#'   or as non-negative integer index into the current
#'   \code{\link[grDevices]{palette}} (with zero being taken as transparent).
#'
#' @seealso \code{\link[ggplot2]{facet_grid}} \code{\link[grid]{unit}}
#'
#' @export
#'
#' @examples
#' setup_cytobands(example_cytobands(),
#'                 example_cytoband_colours())
#'
#' p <- ggplot(mpg, aes(displ, cty)) +
#'    geom_point() +
#'    facet_wrap(~ "chr1")
facet_ideowrap <- function(
  facets, nrow = NULL, ncol = NULL, scales = "fixed",
  shrink = TRUE, labeller = "label_value", as.table = TRUE,
  switch = NULL, drop = TRUE, dir = "h", strip.position = "top",
  ideo.size = unit(0.1, "null"), high.col = NA
) {
  # Error handling
  if (!exists("tbcache", mode = "environment")) {
    stop("No ideograms were found. Please call 'setup_ideograms()' first.",
         call. = FALSE)
  }
  if (!exists("FacetIdeoGrid", envir = tbcache)) {
    stop("No ideograms were found. Please call 'setup_ideograms()' first.",
         call. = FALSE)
  }
  if (!(class(ideo.size) == "unit")) {
    stop("Invalid 'ideo.size' specification.
         Please use 'grid::unit()' to set an appropriate size")
  }
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  dir <- match.arg(dir, c("h", "v"))
  free <- list(x = any(scales %in% c("free_x", "free")),
               y = any(scales %in% c("free_y", "free")))
  if (!is.null(switch)) {
    .Deprecated("strip.position", old = "switch")
    strip.position <- if (switch == "x")
      "bottom"
    else "left"
  }
  strip.position <- match.arg(strip.position,
                              c("top", "bottom", "left", "right"))
  if (identical(dir, "v")){
    nrow_swap <- ncol
    ncol_swap <- nrow
    nrow <- .int$sanitise_dim(nrow_swap)
    ncol <- .int$sanitise_dim(ncol_swap)
  } else {
    nrow <- .int$sanitise_dim(nrow)
    ncol <- .int$sanitise_dim(ncol)
  }
  labeller <- .int$check_labeller(labeller)
  facets_list <- .int$as_facets_list(facets)
  facets <- rlang::flatten_if(facets_list, rlang::is_list)
  ggproto(
    NULL, get("FacetIdeoWrap", envir = tbcache), shrink = shrink,
    params = list(
      facets = facets, free = free, as.table = as.table,
      strip.position = strip.position, drop = drop, ncol = ncol,
      nrow = nrow, labeller = labeller, dir = dir, high.col = high.col,
      ideo.size = ideo.size, high.col = high.col
    )
  )
}

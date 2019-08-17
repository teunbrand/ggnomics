#' Lay out panels in a grid with ideograms
#'
#' \code{facet_ideogrid()} mimicks behaviour of
#' \code{\link[ggplot2]{facet_grid}} but adds ideograms. The facetting variables
#' are checked for chromosome names and if these are found, then ideograms are
#' plotted in between the strips and panels.
#'
#' @inheritParams ggplot2::facet_grid
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
#'    facet_ideogrid(~ "chr1")
facet_ideogrid <- function(
  rows = NULL, cols = NULL, scales = "fixed",
  space = "fixed", shrink = TRUE, labeller = "label_value",
  as.table = TRUE, switch = NULL, drop = TRUE, margins = FALSE,
  facets = NULL, ideo.size = unit(0.1, "null"), high.col = NA
) {
  # Error handling
  if (!exists("tbcache", mode = "environment")) {
    stop("No ideograms were found.
         Please call 'setup_ideograms()' first.", call. = FALSE)
  }
  if (!exists("FacetIdeoGrid", envir = tbcache)) {
    stop("No ideograms were found.
         Please call 'setup_ideograms()' first.", call. = FALSE)
  }
  if (!(class(ideo.size) == "unit")) {
    stop("Invalid 'ideo.size' specification.
         Please use 'grid::unit()' to set an appropriate size")
  }

  if (!is.null(facets)) {
    rows <- facets
  }
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  # Scales
  scales <- match.arg(scales, c("fixed", "free_x", "free_y", "free"))
  free <- list(x = any(scales %in% c("free_x", "free")),
               y = any(scales %in% c("free_y", "free")))
  # Space
  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(x = any(space %in% c("free_x", "free")),
                     y = any(space %in% c("free_y", "free")))

  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }
  facets_list <- .int$grid_as_facets_list(rows, cols)
  n <- length(facets_list)
  if (n > 2L) {
    stop("A grid facet specification can't have more than two dimensions",
         call. = FALSE)
  }
  if (n == 1L) {
    rows <- quos()
    cols <- facets_list[[1]]
  }
  else {
    rows <- facets_list[[1]]
    cols <- facets_list[[2]]
  }
  labeller <- .int$check_labeller(labeller)

  ggproto(
    NULL, get("FacetIdeoGrid", envir = tbcache), shrink = shrink,
    params = list(
      rows = rows, cols = cols, margins = margins, free = free,
      space_free = space_free, labeller = labeller, as.table = as.table,
      switch = switch, drop = drop, ideo.size = ideo.size, high.col = high.col
    )
  )
}

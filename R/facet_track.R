# Main function -----------------------------------------------------------

#' Lay out panels as tracks
#'
#' \code{facet_track} acts much like \code{facet_grid}, but looks for fixed
#' facetting variables in each layer's data. Furthermore, allows panel
#' dimensions can be set.
#'
#' @inheritParams ggplot2::facet_grid
#' @param heights a \code{numeric} or \code{unit} vector for setting panel
#'   heights.
#' @param widths a \code{numeric} or \code{unit} vector for setting panel
#'   widths.
#' @param respect a \code{logical} value. If \code{TRUE}, widths and heights
#'   specified in \code{"null"} units are proportional. If \code{FALSE},
#'   \code{"null"} units in the x- and y-direction vary independently.
#'
#' @details The primary use case for \code{facet_track} is to be used in
#'   combination with the \code{\link[ggnomics]{tracklayer}} function and
#'   allowing panel specification to be independent of the \code{data} arguments
#'   of geoms and stats. \code{facet_track} looks in the layer data for the
#'   columns named \code{"track_x"} and \code{"track_y"} to evaluate facet
#'   specification.
#'
#'   When \code{heights} or \code{widths} are \code{numeric} vectors, panel
#'   sizes are defined as ratios i.e. relative \code{"null"} units.
#'   \code{heights} and \code{widths} vectors are repeated or shortened to fit
#'   the number of panels in their directions. When \code{heights} or
#'   \code{widths} are \code{NULL}, no changes from the default settings are
#'   made in that directions. This can be convenient when '\code{space}' is set
#'   to \code{"free_*"}. Setting the sizes of panels overrides any aspect ratios
#'   set in the theme and \code{coord} functions.
#'   
#' @export
#'
#' @examples
#' ggplot() +
#'   tracklayer("point", data = mtcars,
#'              mapping = aes(disp, mpg),
#'              track_x = "mtcars") +
#'   tracklayer("point", data = iris,
#'              mapping = aes(Sepal.Width, Sepal.Length),
#'              track_x = "iris") +
#'   facet_track(widths = c(2, 1))
facet_track <- function(heights = NULL, widths = NULL, respect = FALSE, scales = "free", 
                        space = "fixed",
                        shrink = TRUE, labeller = "label_value", as.table = TRUE,
                        switch = "y", drop = TRUE, margins = FALSE) {
  track_y <- as.symbol("track_y")
  track_x <- as.symbol("track_x")
  rows <- vars("track_y" = track_y)
  cols <- vars("track_x" = track_x)
  if (is.logical(cols)) {
    margins <- cols
    cols <- NULL
  }
  if (!inherits(heights, "unit") && !is.null(heights)) {
    heights <- unit(heights, "null")
  }
  if (!inherits(widths, "unit") && !is.null(widths)) {
    widths <- unit(widths, "null")
  }
  
  scales <- match.arg(scales, c("fixed", "free_x", "free_y",
                                "free"))
  free <- list(x = any(scales %in% c("free_x", "free")),
               y = any(scales %in% c("free_y", "free")))
  space <- match.arg(space, c("fixed", "free_x", "free_y", "free"))
  space_free <- list(x = any(space %in% c("free_x", "free")),
                     y = any(space %in% c("free_y", "free")))
  if (!is.null(switch) && !switch %in% c("both", "x", "y")) {
    stop("switch must be either 'both', 'x', or 'y'", call. = FALSE)
  }
  facets_list <- .int$grid_as_facets_list(rows, cols)
  labeller <- .int$check_labeller(labeller)
  ggproto(NULL, FacetTrack, shrink = shrink,
          params = list(rows = facets_list$rows,
                        cols = facets_list$cols,
                        heights = heights,
                        widths = widths,
                        respect = respect,
                        margins = margins,
                        free = free,
                        space_free = space_free,
                        labeller = labeller,
                        as.table = as.table,
                        switch = switch, drop = drop))
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggnomics_extensions
FacetTrack <- ggproto(
  "FacetTrack", FacetGrid,
  setup_params = function(data, params) {

    rows <- params$rows
    cols <- params$cols
    
    colvals <- lapply(data, function(dat) {
      if (NROW(dat) == 0 || is.null(dat) || inherits(dat, "waiver") ||
          !("track_x" %in% names(dat))) {
        return("Default")
      } else {
        return(as.character(unique(dat[["track_x"]])))
      }
    })
    colvals <- unlist(colvals)
    
    rowvals <- lapply(data, function(dat) {
      if (NROW(dat) == 0 || is.null(dat) || inherits(dat, "waiver") ||
          !("track_y" %in% names(dat))) {
        return("Default")
      } else {
        return(as.character(unique(dat[["track_y"]])))
      }
    })
    rowvals <- unlist(rowvals)
    
    if (length(unique(rowvals)) == 1 && rowvals[[1]] == "Default") {
      rows <- quos()
    }
    if (length(unique(colvals)) == 1 && colvals[[1]] == "Default") {
      cols <- quos()
    }
    
    params$rows <- rows
    params$cols <- cols
    params
  },
  draw_panels = function(panels, layout, x_scales, y_scales, ranges,
                         coord, data, theme, params, self) {
    gt <- ggproto_parent(FacetGrid, self)$draw_panels(
      panels, layout, x_scales, y_scales, ranges, coord,
      data, theme, params
    )
    
    prows <- panel_rows(gt)$t
    pcols <- panel_cols(gt)$l
    
    if (!is.null(params$heights)) {
      h <- rep(params$heights, length.out = length(prows))
      gt$heights[prows] <- h
    }
    
    if (!is.null(params$widths)) {
      w <- rep(params$widths, length.out = length(pcols))
      gt$widths[pcols]  <- w
    }
    
    gt$respect <- params$respect
    
    gt
  }
)


# Main function -----------------------------------------------------------

#' Segregrating overlapping ranges
#'
#' @description One-dimensional ranged data in the x-direction is segregated in
#'   the y-direction such that no overlap in twodimensional space occurs. This
#'   positioning works best when no relevant information is plotted in the
#'   y-direction.
#'
#' @param extend a \code{numeric} of length 1 indicating how far a range should
#'   be extended in total for calculating overlaps. Setting this argument to a
#'   positive number leaves some space between ranges in the same bin.
#' @param stepsize a \code{numeric} of length 1 that determines how much space
#'   is added between bins in the y-direction. A positive value grows the bins
#'   from bottom to top, while a negative value grows the bins from top to
#'   bottom.
#'
#' @export
#'
#' @details An object is considered disjoint from a second object when the range
#'   between their \code{xmin} and \code{xmax} coordinates don't overlap.
#'   Objects that overlap are assigned to different bins in the y-direction,
#'   whereby lower bins are filled first. This way, information in the
#'   x-direction is preserved and different objects can be discerned.
#'
#'   Note that this positioning is only particularly useful when y-coordinates
#'   do not encode relevant information. Geoms that pair well with this
#'   positioning are \code{\link[ggplot2]{geom_rect}},
#'   \code{\link[ggplot2]{geom_tile}}.
#'
#'   This positioning function was inspired by the \code{disjointBins()}
#'   function in the \code{IRanges} package, but has been written such that it
#'   accepts any numeric input next to solely integer input.
#'
#' @seealso \code{\link[IRanges:inter-range-methods]{disjointBins}}
#'
#' @examples
#' # Even though geom_tile() is parametrised by middle-x values, it is
#' # internally converted to xmin, xmax, ymin, ymax parametrisation so the
#' # positioning still works.
#'
#' ggplot() +
#'   geom_tile(aes(x = rnorm(200), y = 0),
#'             width = 0.2, height = 0.9,
#'             position = position_disjoint_ranges(extend = 0.1))
position_disjoint_ranges <- function(extend = 1, stepsize = 1) {
  ggproto(NULL, PositionDisjointRanges, extend = extend, stepsize = stepsize)
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @importFrom IRanges punion disjointBins
#' @importFrom GenomicRanges absoluteRanges
#' @describeIn ggnomics_extensions See
#'   \code{\link[ggnomics]{position_disjoint_ranges}}.
PositionDisjointRanges <- ggplot2::ggproto(
  "PositionDisjointRanges",
  ggplot2::Position,
  extend   = NULL,
  stepsize = NULL,
  setup_params = function(self, data) {
    params <- list(extend = self$extend,
                   stepsize = self$stepsize)

    has_x <- !is.null(data$x)
    has_y <- !is.null(data$y)
    has_xminmax <- !(is.null(data$xmin) || is.null(data$xmax))
    has_yminmax <- !(is.null(data$ymin) || is.null(data$ymin))

    if (has_x && !has_xminmax) {
      params$mono_x <- TRUE
      if (inherits(Nightfall(data$x), "Ranges")) {
        params$ranged_x <- TRUE
        params$genomic <- inherits(Nightfall(x), "ANYGenomic")
      }
    } else {
      params$mono_x <- FALSE
      xclasses <- intersect(is(Nightfall(data$xmin)), is(Nightfall(data$xmax)))
      params$ranged_x <- "Ranges" %in% xclasses
      params$genomic <- "ANYGenomic" %in% xclasses
      if (HelenOfTroy(data$xmin) != HelenOfTroy(data$xmax)) {
        stop("Position can't combine these types of xmin/xmax")
      }
    }
    params
  },
  compute_panel = function(data, params, scales) {
    # browser()

    group <- data[["group"]]
    if (no_unique <- length(unique(group)) == 1 && group[1] == -1) {
      group <- row(data)[, 1]
    }

    tmp <- list(xmin = Nightfall(data$xmin %||% data$x),
                xmax = Nightfall(data$xmax %||% data$x))
    if (params$ranged_x) {
      x <- punion(tmp$xmin, tmp$xmax)
      if (inherits(x, "ANYGenomic")) {
        # Groups shouldn't cross seqlevels
        group <- interaction(
          group, decode(seqnames(tmp$x %||% tmp$xmin)), drop = TRUE
        )
        maxs <- range(x)
        maxs <- setNames(end(maxs) + params$extend + 1, decode(seqnames(maxs)))
        slvl <- pmax(seqlengths(x), 0, na.rm = T)
        slvl[names(maxs)] <- maxs
        seqlengths(x) <- slvl
        x <- absoluteRanges(x)
      }
      tmp <- list(xmin = start(x), xmax = end(x))
    } else {
      tmp <- list(xmin = pmin(tmp$xmin, tmp$xmax),
                  xmax = pmax(tmp$xmin, tmp$xmax))
    }
    tmp$group <- group
    class(tmp) <- "data.frame"
    attr(tmp, "row.names") <- .set_row_names(length(group))

    # Simplify groups to ranges
    if (!no_unique) {
      # group <- data$group
      tmp <- by(tmp, group, function(dat){
        c(min(dat$xmin), max(dat$xmax), dat$group[1])
      })
      tmp <- do.call(rbind, tmp)

      tmp <- setNames(as.data.frame(tmp),
                         c("xmin", "xmax", "group"))
    }

    # Extend and sort ranges
    tmp$xmin <- tmp$xmin - 0.5 * params$extend
    tmp$xmax <- tmp$xmax + 0.5 * params$extend
    ord <- order(tmp$xmin)
    tmp <- tmp[ord, ]

    # Perform disjoint bins operation similar to IRanges::disjointBins(), but
    # generalized to any ranged numeric data, not just integers.
    track_bins <- tmp$xmax[1]
    tmp$bin <- c(1, vapply(tail(seq_along(ord), -1), function(i) {
      dat <- tmp[i, ]
      j <- which(track_bins < dat$xmin)
      if (length(j) > 0) {
        ans  <- j[1]
        # If a bin is available, update bin
        ends <- track_bins
        ends[ans]  <- dat$xmax
        track_bins <<- ends
      } else {
        # Else, make new bin
        track_bins <<- c(track_bins, dat$xmax)
        ans <- length(track_bins)
      }
      return(ans)
    }, integer(1)))

    # Transform
    map <- match(group, tmp$group)
    if (all(c("ymin", "ymax") %in% names(data))) {
      data$ymax <- data$ymax + params$stepsize * (tmp$bin[map] - 1)
      data$ymin <- data$ymin + params$stepsize * (tmp$bin[map] - 1)
    }
    if ("y" %in% names(data)) {
      data$y <- data$y + params$stepsize * (tmp$bin[map] - 1)
    }

    return(data)
  }
)

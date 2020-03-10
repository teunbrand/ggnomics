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
#' @param dir A \code{character} of length 1, either \code{"x"} or \code{"y"},
#'   naming the direction to consider the range. \code{NULL} (the default) tries
#'   to sniff which direction to take, but internally defaults to \code{"x"}
#'   when it can't decide
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
#' @return A \code{Position} ggproto object.
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
position_disjoint_ranges <- function(extend = 1, stepsize = 1, dir = NULL) {
    if (!is.null(dir)) {
        dir <- match.arg(dir, c("x", "y"))
    }
    ggproto(NULL, PositionDisjointRanges, extend = extend, stepsize = stepsize,
            dir = dir)
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
        dir <- self$dir
        if (is.null(dir)) {
            x_aes <- intersect(names(data), .glob$x_aes)
            y_aes <- intersect(names(data), .glob$y_aes)
            has_x <- length(x_aes) > 1L
            has_y <- length(y_aes) > 1L
            if (!has_y) {
                params$dir <- "x"
            } else if (!has_x) {
                params$dir <- "y"
            } else {
                x_is_ranged <- any(vapply(data[x_aes], HelenOfTroy,
                                          logical(1), what = "Ranges"))
                y_is_ranged <- any(vapply(data[y_aes], HelenOfTroy,
                                          logical(1), what = "Ranges"))
                if (x_is_ranged & !y_is_ranged) {
                    params$dir <- "x"
                } else if (!x_is_ranged & y_is_ranged) {
                    params$dir <- "y"
                } else {
                    x_minmax <- all(c("xmin", "xmax") %in% x_aes)
                    y_minmax <- all(c("ymin", "ymax") %in% y_aes)
                    if (x_minmax & !y_minmax) {
                        params$dir <- "x"
                    } else if (!x_minmax & y_minmax) {
                        params$dir <- "y"
                    } else {
                        xends <- all(c("x", "xend") %in% x_aes)
                        yends <- all(c("y", "yend") %in% x_aes)
                        if (xends & !yends) {
                            params$dir <- "x"
                        } else if (!xends & yends) {
                            params$dir <- "y"
                        } else {
                            # Defaulting to x
                            params$dir <- "x"
                        }
                    }
                }
            }
        } else {
            params$dir <- dir
        }
        params$no_unique <- length(unique(data$group)) == 1 &&
            data$group[1] == -1
        params
    },
    setup_data = function(self, data, params) {
        .int$check_required_aesthetics(self$required_aes, names(data),
                                       .int$snake_class(self))
        mygroup <- data[["group"]]
        if (params$no_unique) {
            mygroup <- row(data)[, 1]
        }

        if (params$dir == "x") {
            tmp <- list(min = Nightfall(data$xmin %||% data$x),
                        max = Nightfall(data$xmax %||% data$xend %||% data$x))
        } else {
            tmp <- list(min = Nightfall(data$ymin %||% data$y),
                        max = Nightfall(data$ymax %||% data$yend %||% data$y))
        }

        # Format ranged data
        if (HelenOfTroy(tmp$min, "Ranges")) {
            x <- punion(tmp$min, tmp$max)
            if (HelenOfTroy(tmp$min, "ANYGenomic")) {
                mygroup <- interaction(
                    mygroup, decode(seqnames(tmp$min)), drop = TRUE
                )
                x <- seqlevels_from_max(x, params$extend + 1)
                x <- absoluteRanges(x)
            }
            tmp <- list(min = start(x), max = end(x))
        } else {
            tmp <- list(min = pmin(tmp$min, tmp$max),
                        max = pmax(tmp$min, tmp$max))
        }

        data <- transform(data,
                          zmin = tmp$min, zmax = tmp$max,
                          altgroup = mygroup)
        data
    },
    compute_panel = function(data, params, scales) {
        tmp <- data[c("altgroup", "zmin", "zmax")]
        tmpgroup <- tmp[["altgroup"]]

        # Simplify groups to ranges
        if (!params$no_unique) {
            tmp <- by(tmp, tmpgroup, function(dat){
                c(min(dat$zmin), max(dat$zmax), dat$altgroup[1])
            })
            tmp <- do.call(rbind, tmp)

            tmp <- setNames(as.data.frame(tmp),
                            c("zmin", "zmax", "altgroup"))
        }

        # Extend and sort ranges
        tmp$zmin <- tmp$zmin - 0.5 * params$extend
        tmp$zmax <- tmp$zmax + 0.5 * params$extend
        ord <- order(tmp$zmin)
        tmp <- tmp[ord, ]

        # Perform disjoint bins operation similar to IRanges::disjointBins(),
        # but generalized to any ranged numeric data, not just integers.
        track_bins <- tmp$zmax[1]
        tmp$bin <- 1L
        for (i in tail(seq_along(ord), -1)) {
            dat <- tmp[i,]
            j <- which(track_bins < dat$zmin)
            if (length(j) > 0) {
                ans <- j[1]
                track_bins[ans] <- dat$zmax
            } else {
                track_bins <- c(track_bins, dat$zmax)
                ans <- length(track_bins)
            }
            tmp$bin[i] <- ans
        }

        # Transform
        map <- match(tmpgroup, tmp$altgroup)
        bin <- tmp$bin[map] - 1
        if (params$dir == "x") {
            my_aes <- intersect(names(data), .glob$y_aes)
        } else {
            my_aes <- intersect(names(data), .glob$x_aes)
        }
        data[my_aes] <- lapply(data[my_aes], function(y) {
            y + params$stepsize * bin
        })

        data <- data[setdiff(colnames(data), c("zmin", "zmax", "altgroup"))]

        return(data)
    }
)

# Helpers -----------------------------------------------------------------

# Sets seqlevels of a GRanges object to the maximum of that seqlevel plus
# some extension.
# Useful when GRanges are to be flattened by absoluteRanges
seqlevels_from_max <- function(x, extend = 0L) {
  maxs <- range(x)
  maxs <- setNames(end(maxs) + extend, decode(seqnames(maxs)))
  slvl <- pmax(seqlengths(x), 0, na.rm = TRUE)
  slvl[names(maxs)] <- maxs
  seqlengths(x) <- slvl
  return(x)
}

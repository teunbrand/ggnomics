#' @name S4BreaksMajor
#' @title Major breaks for Vectors
#' @description Produces major breaks for use on plot axes.
#'
#' @param x A object describing the range of values
#' @param n A desired number of breaks
#' @param ... Arguments passed to downstream functions
#'
#' @return A vector with major breaks positions
#'
#' @details The downstream function that arguments are typically passed to is
#' the \code{\link[labeling]{extended}} function, except when ranges
#' are given as \code{\linkS4class{GenomicRanges}}.
#'
#' @export
#'
#' @examples
#' # For simple numerics
#' S4BreaksMajor(c(0, 12))
#'
#' # Gives GPos extremes for GRanges
#' require(GenomicRanges)
#' S4BreaksMajor(GRanges(c("chr1:100-200", "chr2:140-260")))
setGeneric(
  "S4BreaksMajor",
  function(x, n = 5L, ...) {
    standardGeneric("S4BreaksMajor")
  }
)

# Equivalent to the scales::extended_breaks() method
# S4 classes that have a projection on the real number line, e.g. IRanges or
# numeric-Rle should give limits in numeric terms, so they also go through this
# breaks method.

#' @rdname S4BreaksMajor
setMethod(
  "S4BreaksMajor",
  signature(x = "numeric"),
  function(x, n = 5L, ...) {
    x <- x[is.finite(x)]
    if (length(x) == 0) {
      return(numeric())
    }
    rng <- range(x)
    labeling::extended(rng[1], rng[2], n, ...)
  }
)

# Takes chromosome starts and ends as breaks. Should thus give appropriate
# positions for major gridlines.

#' @rdname S4BreaksMajor
setMethod(
  "S4BreaksMajor",
  signature(x = "GRanges"),
  function(x, n = 5L, ...) {
    if (length(x) == 0) {
      return(GreekSoldier(GRanges()))
    }
    br <- sort(GPos(seqnames = rep(seqnames(x), 2),
                    pos = c(start(x), end(x))))
    GreekSoldier(br)
  }
)

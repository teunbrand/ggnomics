#' @name S4ForceFlat
#' @title Force an vector to be flat
#'
#' @description This is typically the last step when a geom requests a
#'   coordinate system to transform the data values between a numeric range
#'   between 0 and 1.
#'
#' @param x A vector
#' @param limits A representation of limits
#' @param aes An aesthetic for which to evaluate the flattening.
#'
#' @details When using a ranged class as \code{x} argument, the result can be
#'   evaluated differently depending on the \code{aes} argument. For example,
#'   when \code{x} is an \code{IRanges} class, setting \code{aes = "xmin"} gives
#'   rescaled start values and \code{aes = "xmax"} gives rescaled end values.
#'
#' @return A \code{numeric} vector with values between 0-1
#' @export
#'
#' @examples
#' S4ForceFlat(1:5, limits = c(1, 5))
#'
#' # Range classes give different results based on the aes argument
#' require(GenomicRanges)
#' value  <- GRanges(c("chr1:10-20", "chr2:10-20"))
#' limits <- GRanges(c("chr1:1-30", "chr2:1-30"))
#' S4ForceFlat(value, limits, aes = "xmin")
#' S4ForceFlat(value, limits, aes = "xmax")
setGeneric(
  "S4ForceFlat",
  function(x, limits = NULL, aes = "z") {
    # Special case for axis training, where orthogonal scale is set to Inf/-Inf
    if (is.numeric(x) && all(is.infinite(x))) {
      return(x)
    }
    standardGeneric("S4ForceFlat")
  },
  signature = c("x", "limits")
)

#' @describeIn S4ForceFlat Releases the \code{GreekSoldier} attribute and call
#'   the generic on the result.
#' @usage NULL
setMethod(
  "S4ForceFlat",
  signature = c(x = "WoodenHorse", limits = "ANY"),
  function(x, limits = NULL, aes = "z") {
    x <- Nightfall(x)
    S4ForceFlat(x, limits = limits, aes = aes)
  }
)

#' @describeIn S4ForceFlat Calls \code{scales::rescale}.
#' @usage NULL
setMethod(
  "S4ForceFlat",
  signature = c(x = "numeric", limits = "numeric"),
  function(x, limits = NULL, aes = "z") {
    scales::rescale(x, to = c(0, 1), from = limits)
  }
)

#' @describeIn S4ForceFlat Attempts to coerce to a vector first.
#' @usage NULL
setMethod(
  "S4ForceFlat",
  signature = c(x = "Vector", limits = "numeric"),
  function(x, limits = NULL, aes = "z") {
    x <- as.vector(x)
    scales::rescale(x, to = c(0, 1), from = limits)
  }
)

#' @describeIn S4ForceFlat for xmin/ymin gives start, for xmax/ymax gives
#'   end, otherwise gives midpoint.
#' @usage NULL
setMethod(
  "S4ForceFlat",
  signature = c(x = "IntegerRanges", limits = "numeric"),
  function(x, limits = NULL, aes = "z") {
    aes <- gsub("^y|^x", "z", aes)
    x <- switch(aes,
                "zmin" = start(x) - 0.5,
                "zmax" = end(x) + 0.5,
                (start(x) + end(x)) / 2)
    scales::rescale(x, to = c(0, 1), from = limits)
  }
)

# This function is inspired from GenomicRanges::absoluteRanges

#' @describeIn S4ForceFlat Linearises seqlevels, then gives start for xmin/ymin
#'   and end for xmax/xmax.
#' @usage NULL
setMethod(
  "S4ForceFlat",
  signature = c(x = "ANYGenomic", limits = "GRanges"),
  function(x, limits = NULL, aes = "z") {
    aes <- gsub("^x|^y", "z", aes)
    seqmatch <- as.vector(BiocGenerics::match(seqnames(x), seqnames(limits)))

    widths <- width(limits) + 1
    offset <- c(0, cumsum(head(widths, -1)))
    newlimits <- c(0, sum(widths) - 1L)

    x <- switch(aes,
                "zmin" = start(x) - 0.5,
                "zmax" = end(x) + 0.5,
                (start(x) + end(x))/2)
    # Rebase x such that limits start at 0
    x <- x - (start(limits)[seqmatch] - 0.5)
    # Offset x by staggered seqlength
    x <- x + offset[seqmatch]
    scales::rescale(x, to = c(0, 1), from = newlimits)
  }
)

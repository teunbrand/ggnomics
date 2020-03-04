# Range calculators -------------------------------------------------------

#' @name S4Range
#' @title Range of Values
#'
#' @description Returns a vector containing the minimum and maximum of all the given
#' arguments.
#'
#' @param x An object to determine the range of.
#' @param ... Optionally, more objects of the same class as \code{x} to include
#'   in the range calculation.
#' @param na.rm \code{logical} of length 1: omit \code{NA} values prior to
#'   calculating ranges?
#' @param finite \code{logical} of length 1: omit non-finite elements prior to
#'   calculating ranges?
#' @param aes An aesthetic for which to evaluate the function.
#'
#' @details Defaults to the \code{\link[base]{range}} function.
#' 
#' @return A vector indicating lower- and upper-bounds of the input.
#' @export
#'
#' @examples
#' # For regular numeric vectors
#' S4Range(c(1:3, -1))
#'
#' # GenomicRanges returns same type
#' require(GenomicRanges)
#' S4Range(GRanges(c("chr1:100-200", "chr2:200-300", "chr2:500-600")))
setGeneric(
  "S4Range",
  function(x, ..., na.rm = FALSE,
           finite = FALSE, aes = "z") standardGeneric("S4Range"),
  signature = "x"
)

#' @rdname S4Range
setMethod(
  "S4Range",
  signature = c(x = "knownContinuous"),
  definition = function(x, ..., na.rm = FALSE, finite = FALSE, aes = "z") {
    range(x, ..., na.rm = na.rm, finite = finite)
  }
)

#' @rdname S4Range
setMethod(
  "S4Range",
  signature = c(x = "Rle"),
  definition = function(x, ..., na.rm = FALSE, finite = FALSE, aes = "z") {
    x <- bindROWS(x, list(...), use.names = FALSE, 
                  ignore.mcols = TRUE, check = FALSE)
    range(runValue(x), na.rm = na.rm, finite = finite)
  }
)

#' @rdname S4Range
setMethod(
  "S4Range",
  signature = c(x = "Ranges"),
  definition = function(x, ..., na.rm = FALSE, finite = FALSE, aes = "z") {
    x <- bindROWS(x, list(...), use.names = FALSE, 
                  ignore.mcols = TRUE, check = FALSE)
    range(c(start(x), end(x)))
  }
)

#' @rdname S4Range
setMethod(
  "S4Range",
  signature = c(x = "GenomicRanges"),
  definition = function(x, ..., na.rm = FALSE, finite = FALSE, aes = "z") {
    x <- bindROWS(granges(x), list(...),
                  use.names = FALSE, ignore.mcols = TRUE, check = FALSE)
    y <- base::vapply(base::split(c(start(x), end(x)), 
                                  decode(rep(seqnames(x), 2)), drop = TRUE),
                      function(z) {c(min(z), max(z))}, integer(2))
    GRanges(colnames(y), IRanges(y[1,], y[2,]), seqinfo = seqinfo(x))
  }
)

#' @rdname S4Range
setMethod(
  "S4Range",
  signature = c("x" = "WoodenHorse"),
  definition = function(x, ..., na.rm = FALSE, finite = FALSE, aes = "z") {
    x <- Nightfall(x)[!is.na(x)]
    dots <- list(...)
    if (length(dots) > 0) {
      dots <- unlist(as(lapply(dots, function(y){Nightfall(y)[!is.na(y)]}), 
                        "List"))
      callGeneric(x, dots)
    } else {
      callGeneric(x)
    }
  }
)

# Range expansion ---------------------------------------------------------

#' @name S4ExpandRange
#' @title Range expansion
#'
#' @description Expand a range with multiplicative and additive constants
#'
#' @param limits An object indicating a range
#' @param expand A \code{numeric} vector of length 4, typically the result of a
#' call to \code{\link[ggplot2]{expansion}}.
#'
#' @details The \code{expand} argument consist of 4 constants that code for the
#'   following:
#'   \enumerate{
#'    \item A multiplicative constant relative to the width of the \code{limits}
#'    argument to subtract from the start of the limits.
#'    \item An additive constant to subtract from the start of the limits.
#'    \item A multiplicative constant relative to the width of the \code{limits}
#'    argument to add to the end of the limits.
#'    \item An additive constant to add to the end of the limits.
#'   }
#'
#' Integer-based classes such as \code{IRanges} and \code{GRanges} are rounded
#' to the nearest integer.
#'
#' @return An object indicating a range
#' @export
#'
#' @examples
#' # Regular numeric limits
#' S4ExpandRange(c(10, 20), expansion(0.5, 1))
#'
#' # IntegerRanges
#' require(IRanges)
#' S4ExpandRange(IRanges::IRanges(10, 20), expansion(0.5, 1))
#'
#' # GenomicRanges
#' require(GenomicRanges)
#' S4ExpandRange(GenomicRanges::GRanges(c("chr1", "chr2"), c("10-20", "20-30")),
#'               expansion(0.5, 1))
setGeneric(
  "S4ExpandRange",
  function(limits, expand = expansion()) standardGeneric("S4ExpandRange"),
  signature = "limits"
)

#' @rdname S4ExpandRange
setMethod(
  "S4ExpandRange",
  signature = c(limits = "NULL"),
  function(limits, expand = expansion()){
    return()
  }
)

#' @rdname S4ExpandRange
setMethod(
  "S4ExpandRange",
  signature = c(limits = "numeric"),
  function(limits, expand = expansion()) {
    lower <- scales::expand_range(limits, expand[1], expand[2])[1]
    upper <- scales::expand_range(limits, expand[3], expand[4])[2]
    c(lower, upper)
  }
)

#' @rdname S4ExpandRange
setMethod(
  "S4ExpandRange",
  signature = c(limits = "Ranges"),
  function(limits, expand = expansion()) {
    if (sum(width(limits)) < 1) {
      width <- 1
    } else {
      width <- width(limits) - 1L
    }
    start <- as.integer(
      round(start(limits) + -1 * (width * expand[1] + expand[2]))
    )
    end <- as.integer(
      round(end(limits) + 1 * (width * expand[3] + expand[4]))
    )
    update_ranges(
      limits,
      start = pmin(start, end),
      end = pmax(start, end)
    )
  }
)

# Zero range --------------------------------------------------------------

#' @name S4ZeroRange
#' @title Check zero rang
#'
#' @description Check if range of limits is close to zero
#'
#' @param x An object representing a range
#' @param tol  A value specifying the tolerance.
#'
#' @return A \code{logical} of length 1: \code{TRUE} if the relative differences
#'   of the range are not distinguishable from 0.
#' @export
#'
#' @details Defaults to the \code{\link[scales]{zero_range}} function.
#'
#' @examples
#' S4ZeroRange(c(10, 10)) # TRUE
#' S4ZeroRange(c(10, 11)) # FALSE
#' S4ZeroRange(c(NA, 10)) # NA
setGeneric(
  "S4ZeroRange",
  function(x, tol = 1000 * .Machine$double.eps) standardGeneric("S4ZeroRange")
)

#' @rdname S4ZeroRange
setMethod(
  "S4ZeroRange",
  signature = c(x = "numeric"),
  definition = function(x, tol = 1000 * .Machine$double.eps) {
    scales::zero_range(x, tol = tol)
  }
)

#' @rdname S4ZeroRange
setMethod(
  "S4ZeroRange",
  signature = c(x = "Ranges"),
  definition = function(x, tol = 1000 * .Machine$double.eps) {
    sum(width(x)) < tol
  }
)

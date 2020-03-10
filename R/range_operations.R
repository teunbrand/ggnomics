# Range calculators -------------------------------------------------------

#' @name S4Range
#' @title Range of Values
#'
#' @description Returns a vector containing the minimum and maximum of all the
#'   given arguments.
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
            dots <- unlist(as(lapply(dots,
                                     function(y){Nightfall(y)[!is.na(y)]}),
                              "List"))
            callGeneric(x, dots)
        } else {
            callGeneric(x)
        }
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

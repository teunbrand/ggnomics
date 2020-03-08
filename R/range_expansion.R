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

#' @name S4Train
#' @title Scale training
#'
#' @description Function for updating the scale range given a new and existing
#'   range.
#'
#' @param new An object representing the current range
#' @param existing An object representing the new range
#' @param ... Arguments passed down to downstream methods. Currently used for
#'   discrete scales.
#' @param drop When \code{TRUE}, will drop factor levels not associated with
#'   data. Used in discrete scales.
#' @param na.rm When \code{TRUE}, will remove missing values. Used in discrete
#'   scales.
#' @param aes An aesthetic for which to evaluate the range
#'
#' @return An updated representation of a range
#' @export
#'
#' @examples
#' # For plain numeric vectors
#' S4Train(new = 1:10, existing = c(-5, -15))
#'
#' # IRanges return plain limits
#' require(IRanges)
#' S4Train(new = IRanges("2501-2900"), existing = c(2000, 2500))
#'
#' # For GenomicRanges
#' require(GenomicRanges)
#' S4Train(new = GRanges(c("chr1:100-200", "chr2:1-2")),
#'         existing = GRanges(c("chr2:200-300")))
setGeneric(
    "S4Train",
    function(new, existing = NULL, drop = FALSE,
             na.rm = FALSE, ..., aes = "z") standardGeneric("S4Train"),
    signature = c("new", "existing")
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "NULL", existing = "ANY"),
    definition = function(new, existing = NULL, ..., aes = "z") {
        return(existing)
    }
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "WoodenHorse", existing = "ANY"),
    definition = function(new, existing = NULL, ..., aes = "z") {
        new <- Nightfall(new, na.rm = TRUE)
        callGeneric()
    }
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "ANY", existing = "ANY"),
    definition = function(new, existing = NULL, ..., aes = "z") {
        S4Range(new, existing, na.rm = TRUE, finite = TRUE, aes = aes)
    }
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "knownDiscretes", existing = "knownDiscretes_OR_missing"),
    definition = function(new, existing = NULL, drop = FALSE,
                          na.rm = FALSE, ..., aes = "z") {
        discrete_training(existing, new, drop = drop, na.rm = na.rm)
    }
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "IntegerRanges", existing = "numeric_OR_missing"),
    definition = function(new, existing = NULL, ..., aes = "z") {
        new <- c(start(new) - 0.5, end(new) + 0.5)
        S4Range(new, existing, aes = aes)
    }
)

# We suppress GRanges warning since if occurs that
# the seqlevels are not common between new and existing
#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "GenomicRanges", existing = "GRanges_OR_missing"),
    definition = function(new, existing = NULL, ..., aes = "z") {
        suppressWarnings(S4Range(new, existing, aes = aes))
    }
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "Rle", existing = "ANY"),
    definition = function(new, existing = NULL, drop = FALSE,
                          na.rm = FALSE, aes = "z") {
        new <- runValue(new)
        callGeneric()
    }
)

#' @rdname S4Train
setMethod(
    "S4Train",
    signature = c(new = "Factor", existing = "ANY"),
    definition = function(new, existing = NULL, drop = FALSE,
                          na.rm = FALSE, aes = "z") {
        new <- levels(new)
        callGeneric()
    }
)

# Helpers -----------------------------------------------------------------

# These are internals from the scales package, using these for
# discrete scale training

discrete_training <- function(old, new, drop = FALSE, na.rm = FALSE) {
    new <- clevels(new, drop = drop, na.rm = na.rm)
    if (is.null(old)) return(new)
    if (!is.character(old)) old <- clevels(old, na.rm = na.rm)
    new_levels <- setdiff(new, as.character(old))
    if (length(new_levels) == 0) {
        return(old)
    }

    sort(c(old, new_levels))
}

clevels <- function(x, drop = FALSE, na.rm = FALSE) {
    if (is.null(x)) {
        character()
    } else if (is.factor(x)) {
        if (drop) x <- factor(x)
        values <- levels(x)
        if(na.rm) {
            values <- values[!is.na(values)]
        } else if (any(is.na(x))) {
            values <- c(values, NA)
        }
        values
    } else {
        sort(unique(x), na.last = if (na.rm) NA else TRUE)
    }
}

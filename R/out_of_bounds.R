#' @include all-class-unions.R
# #' @include force_flat.R
NULL

# Out of bounds -----------------------------------------------------------

# Until the scales package makes their out-of-bounds functions generic, I'm
# implementing parallel functions otherwise ggplot's internals are going to
# use the non-generic version anyway.

#' @noRd
#' @keywords internal
#' @title Out of bounds
#'
#' @description Check if vector is out of bounds
#'
#' @param x The vector to check
#' @param range The bounds to which to compare
#' @param ... Not implemented
#'
#' @return A logical
#'
#' @examples
#' is_oob(1:10, c(5,7))
setGeneric(
  "is_oob",
  function(x, range = c(0, 1), ...) standardGeneric("is_oob"),
  signature = c("x", "range"),
  valueClass = "logical"
)

setMethod(
  "is_oob",
  signature = c(x = "ANY", range = "numeric"),
  function(x, range) {
    as.vector(x < range[1] | x > range[2])
  }
)

setMethod(
  "is_oob",
  signature = c(x = "IntegerRanges", range = "numeric"),
  function(x, range) {
    start(x) < range[1] | end(x) > range[2]
  }
)

setMethod(
  "is_oob",
  signature = c(x = "ANYGenomic", range = "GRanges"),
  function(x, range) {
    seqmatch <- as.vector(match(seqnames(x), seqnames(range)))
    start(x) < start(range)[seqmatch] | end(x) > end(range)[seqmatch]
    # !overlapsAny(x, range, type = "within")
  }
)

# Censoring ---------------------------------------------------------------

# Generic and methods to do the same as scales::censor

#' @name censorThis
#' @title Censor any values outside of range
#'
#' @description This function follows the \code{\link[scales]{censor}} function,
#'   but allows extensions that work with objects other than numeric vectors.
#'
#' @param x A vector of values to manipulate.
#' @param range A range representing object.
#' @param only.finite If \code{TRUE} (the default), will only modify finite
#'   values.
#' @param aes An aesthetic for which to evaluate the function.
#'
#' @return The \code{x} argument, but with an indication which values should be
#'   censored.
#' @export
#'
#' @examples
#' # For regular numeric vectors
#' censorThis(c(-1, 0.5, 1, 2, NA))
setGeneric(
  "censorThis",
  function(x, range = NULL,
           only.finite = TRUE, aes = "z") standardGeneric("censorThis"),
  signature = c("x", "range")
  )

# WoodenHorse forwards the problem to is_oob
#' @rdname censorThis
#' @aliases censorThis
setMethod(
  "censorThis",
  signature = c(x = "WoodenHorse"),
  definition = function(x, range = c(0, 1), only.finite = TRUE, aes = "z") {
    setNA(x, is_oob(Nightfall(x), range))
  }
)

#' @rdname censorThis
setMethod(
  "censorThis",
  signature = c(x = "numeric", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), only.finite = TRUE, aes = "z") {
    scales::censor(x = x, range = range, only.finite = only.finite)
  }
)

# Squishing ---------------------------------------------------------------

# Generic and methods to do the same as scales::squish

#' @name squishThis
#' @title Squish values into range
#'
#' @description This function follows the \code{\link[scales]{squish}} function, but allows
#' extentions that work with objects other than numeric vectors.
#'
#' @inheritParams censorThis
#'
#' @return The \code{x} argument, but with out-of-bounds values squished to the
#'   range.
#' @export
#'
#' @examples
#' # For regular numeric vectors
#' squishThis(c(-1, 0.5, 1, 2, NA))
#'
#' # For Rle-class
#' squishThis(Rle(1:4, 4:1), c(2,3))
setGeneric(
  "squishThis",
  function(x, range = NULL,
           only.finite = TRUE, aes = "z") standardGeneric("squishThis"),
  signature = c("x", "range")
)

# WoodenHorse unpacks and re-packs before and after squishing leaving
# the details up to class-specific methods

#' @rdname squishThis
setMethod(
  "squishThis",
  signature = c(x = "WoodenHorse"),
  definition = function(x, range = c(0, 1), only.finite = TRUE, aes = "z") {
    dat <- vec_data(x)
    xclass <- setdiff(class(x), "vctrs_vctr")
    y <- callGeneric(attr(x, 'GreekSoldier'), range)
    if ("hsh" %in% names(attributes(x))) {
      new_vctr(
        dat,
        hsh = digest(y),
        GreekSoldier = y,
        class = xclass
      )
    } else {
      new_vctr(
        dat,
        GreekSoldier = y,
        class = xclass
      )
    }
  }
)

#' @rdname squishThis
setMethod(
  "squishThis",
  signature = c(x = "numeric", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), only.finite = TRUE, aes = "z") {
    scales::squish(x = x, range = range, only.finite = only.finite)
  }
)

#' @rdname squishThis
setMethod(
  "squishThis",
  signature = c(x = "Rle", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), only.finite = TRUE, aes = "z") {
    runValue(x) <- scales::squish(runValue(x), range = range,
                                  only.finite = only.finite)
    x
  }
)

#' @rdname squishThis
setMethod(
  "squishThis",
  signature = c(x = "IntegerRanges", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), only.finite = TRUE, aes = "z") {
    IRanges(pmax.int(pmin.int(start(x), range[2]), range[1]),
            pmin.int(pmax.int(end(x), range[1] -1), range[2] -1))
  }
)

#' @rdname squishThis
setMethod(
  "squishThis",
  signature = c(x = "ANYGenomic", range = "GRanges"),
  definition = function(x, range = GRanges(NA_character_, IRanges(0, 1))) {
    seqmatch <- match(seqnames(x), seqnames(range))
    granges(pintersect(x, range[seqmatch], ignore.strand = TRUE))
  }
)

# Squishing infinites -----------------------------------------------------

# Generic and methods to do the same as scales::squish_infinite

#' @name squish_infiniteThis
#' @title  Squish infinite values to range
#'
#' @description This function follows the \code{\link[scales]{squish_infinite}}
#'   function, but allows extentions that work with other objects than numeric
#'   vectors.
#'
#' @inheritParams censorThis
#'
#' @return The \code{x} argument, but with infinite values squished to the
#'   range.
#' @export
#'
#' @examples
#' # For regular numeric vectors
#' squish_infiniteThis(c(-Inf, -1, 0, 1, 2, Inf))
#'
#' # For Rle-class
#' squish_infiniteThis(Rle(c(Inf, 2, 3, -Inf), 1:4), c(2, 3))
setGeneric("squish_infiniteThis", function(x, range = NULL, aes = "z") {
  standardGeneric("squish_infiniteThis")
})

#' @rdname squish_infiniteThis
setMethod(
  "squish_infiniteThis",
  signature = c(x = "numeric", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), aes = "z") {
    scales::squish_infinite(x = x, range = range)
  }
)

#' @rdname squish_infiniteThis
setMethod(
  "squish_infiniteThis",
  signature = c(x = "Rle", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), aes = "z") {
    runValue(x) <- scales::squish_infinite(runValue(x), range = range)
    x
  }
)

#' @rdname squish_infiniteThis
setMethod(
  "squish_infiniteThis",
  signature = c(x = "WoodenHorse", range = "numeric_OR_missing"),
  definition = function(x, range = c(0, 1), aes = "z") {
    dat <- vec_data(x)
    xclass <- setdiff(class(x), "vctrs_vctr")
    y <- callGeneric(attr(x, 'GreekSoldier'), range)
    if ("hsh" %in% names(attributes(x))) {
      new_vctr(
        dat,
        hsh = digest(y),
        GreekSoldier = y,
        class = xclass
      )
    } else {
      new_vctr(
        dat,
        GreekSoldier = y,
        class = xclass
      )
    }
  }
)

# Discarding out-of-bounds ------------------------------------------------

#' @name discardOob
#' @title Discard out of bounds values
#'
#' @description Typically calls \code{\link[ggnomics]{censorThis}} on the
#'   values and subsequently discards \code{NA} values.
#'
#' @inheritParams censorThis
#'
#' @return The values in \code{x} but with out of bounds values discarded.
#' @export
#'
#' @examples
#' # Regular numeric vectors
#' discardOob(c(0:5), c(2, 4))
setGeneric("discardOob", function(x, range = NULL, aes = "z") {
  if (is.null(x)) {
    return(NULL)
  }
  standardGeneric("discardOob")
})

#' @rdname discardOob
setMethod(
  "discardOob",
  signature = c(x = "ANY", range = "missing"),
  definition = function(x, range, aes = "z") {
    x <- censorThis(x, only.finite = FALSE, aes = aes)
    x[!is.na(x)]
  }
)

#' @rdname discardOob
setMethod(
  "discardOob",
  signature = c(x = "numeric", range = "numeric"),
  definition = function(x, range = c(0, 1), aes = "z") {
    x <- censorThis(x, range = range, only.finite = FALSE, aes = aes)
    x[!is.na(x)]
  }
)

#' @rdname discardOob
setMethod(
  "discardOob",
  signature = c(x = "GenomicRanges", range = "GenomicRanges"),
  definition = function(x, range = GRanges(NA_character_,
                                           IRanges(0, 1)),
                        aes = "z") {
    x[!overlapsAny(x, range)]
  }
)

#' @rdname discardOob
setMethod(
  "discardOob",
  signature = c(x = "WoodenHorse", range = "ANY"),
  definition = function(x, range = c(0, 1), aes = "z") {
    x <- Nightfall(x, na.rm = TRUE)
    x <- x[!is_oob(x, range)]
    x <- GreekSoldier(x)
  }
)

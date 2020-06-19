# Documentation -----------------------------------------------------------

#' @name TheFallOfTroy
#' @aliases GreekSoldier Nightfall HelenOfTroy
#' @title Hiding S4 objects as S3 vectors
#'
#' @description There are some limitations to getting S4 objects past the
#'   internals of the ggplot2 system. One of these is a hard check with
#'   \code{rlang::is_vector} on data.frame columns, which will throw an error
#'   when the column is an S4 object. To work around these limitations, we
#'   disguise S4 Vectors classes as S3 vectors from the vctrs package.
#'
#' @param x For \code{GreekSoldier()}, an S4 Vector class object.\cr For
#'   \code{Nightfall()} and \code{HelenOfTroy()}, an S3 \code{WoodenHorse} class
#'   object.
#' @param na.rm For \code{Nightfall()}, should the result be returned for non-NA
#'   entries?
#' @param what For \code{HelenaOfTroy}, an optional class name to check for
#'   inheritance.
#'
#' @details Calling \code{GreekSoldier()} on an S4 Vector object will generate
#'   an object of the class \code{WoodenHorse} of the same length as the input,
#'   where the original S4 Vector is inside the \code{WoodenHorse} object as an
#'   attribute.
#'
#'   Encoding the \code{WoodenHorse} object as a \code{vctrs_vctr} object allows
#'   us to pass the \code{rlang::is_vector} check and gives us nice options to
#'   preserve attributes over subsetting and concatenation operations.
#'
#' @return For \code{Nightfall()}, a \code{Vector} object. For
#'   \code{GreekSoldier()}, a \code{WoodenHorse} object. For
#'   \code{HelenaOfTroy()}, a \code{character},
#'
#' @examples
#' # Making an S4 object
#' Anticlus <- Rle(1:10, 10:1)
#'
#' # Rle object does not pass is_vector test
#' rlang::is_vector(Anticlus)
#'
#' # Disguising the object as S3 vector
#' VictoryTrophy <- GreekSoldier(Anticlus)
#' class(VictoryTrophy) # A WoodenHorse
#'
#' # WoodenHorse passes is_vector test
#' rlang::is_vector(VictoryTrophy)
#'
#' # Inspecting the class of WoodenHorse
#' HelenOfTroy(VictoryTrophy) # Rle
#'
#' # Restoring WoodenHorse to S4 object
#' Nightfall(VictoryTrophy)
NULL

# Greek Soldier -----------------------------------------------------------

#' @rdname TheFallOfTroy
#' @export
setGeneric(
    "GreekSoldier",
    function(x) {
        standardGeneric("GreekSoldier")
    }
)

#' @describeIn TheFallOfTroy By default, don't disguise object as
#'   \code{WoodenHorse} and return the input.
#' @usage NULL
setMethod(
    "GreekSoldier",
    signature = c(x = "ANY"),
    function(x) x
)

#' @describeIn TheFallOfTroy If an object inherits from \linkS4class{Vector},
#'   disguise object as \code{WoodenHorse}, and put the input as an attribute of
#'   the vector.
#' @usage NULL
setMethod(
    "GreekSoldier",
    signature = c(x = "Vector"),
    definition = function(x) BeechHorse(x)
)

#' @rdname TheFallOfTroy
#' @usage NULL
setMethod(
    "GreekSoldier",
    signature = c(x = "AtomicList"),
    definition = function(x) {
        nas <- vapply(x, function(x){any(is.na(x))}, logical(1))
        new_vctr(
            c(0, NA_real_)[nas + 1],
            class = c("BeechHorse", "WoodenHorse"),
            GreekSoldier = x
        )
    }
)

#' @rdname TheFallOfTroy
#' @usage NULL
setMethod(
    "GreekSoldier",
    signature = c(x = "ANYGenomic"),
    definition = function(x) OakHorse(x)
)

#' @rdname TheFallOfTroy
#' @usage NULL
setMethod(
    "GreekSoldier",
    signature = c(x = "matrix"),
    definition = function(x) BeechHorse(x)
)

# Night falls over Troy ---------------------------------------------------

#' @rdname TheFallOfTroy
#' @export
setGeneric(
    "Nightfall",
    function(x, na.rm = FALSE) {
        standardGeneric("Nightfall")
    }
)

#' @describeIn TheFallOfTroy If the object is not a \code{WoodenHorse} return
#'   the input unaltered.
#' @usage NULL
setMethod(
    "Nightfall",
    signature = c(x = "ANY"),
    function(x, na.rm = FALSE) x
)

# Return GreekSoldier if WoodenHorse

#' @describeIn TheFallOfTroy If the object is a \code{WoodenHorse} return the
#'   \code{GreekSoldier} attribute within the \code{WoodenHorse}.
#' @usage NULL
setMethod(
    "Nightfall",
    signature = c(x = "BeechHorse"),
    function(x, na.rm = FALSE) {
        if (isTRUE(na.rm)) {
            extractROWS(attr(x, "GreekSoldier"), i = which(!is.na(x)))
        } else {
            attr(x, "GreekSoldier")
        }
    }
)

#' @describeIn TheFallOfTroy If the object is a \code{WoodenHorse} return the
#'   \code{GreekSoldier} attribute within the \code{WoodenHorse}.
setMethod(
    "Nightfall",
    signature = c(x = "OakHorse"),
    function(x, na.rm = FALSE) {
        if (isTRUE(na.rm)) {
            i <- vec_data(x)[!is.na(x)]
        } else {
            i <- vec_data(x)
        }
        extractROWS(attr(x, "GreekSoldier"), i = i)
    }
)

# Helen of Troy ----------------------------------------------------------

# Supposedly, Helen of Troy imitated the voices of the wives of the Greek
# soldiers in an attempt to reveal the contents of the Trojan Horse.

#' @rdname TheFallOfTroy
#' @export
HelenOfTroy <- function(x, what = NULL) {
    if (is.null(x)) {
        return(class(x)[[1]])
    }
    if (is.null(what)) {
        if (inherits(x, "WoodenHorse")) {
            return(class(attr(x, "GreekSoldier"))[[1]])
        } else {
            return(class(x)[[1]])
        }
    } else {
        if (inherits(x, "WoodenHorse")) {
            return(inherits(attr(x, "GreekSoldier"), what))
        } else {
            return(inherits(x, what))
        }
    }
}

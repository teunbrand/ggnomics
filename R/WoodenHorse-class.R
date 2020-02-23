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
#'
#' @details Calling \code{GreekSoldier()} on an S4 Vector object will generate
#'   an object of the class \code{WoodenHorse} of the same length as the input,
#'   where the original S4 Vector is inside the \code{WoodenHorse} object as an
#'   attribute.
#'
#'   Calling \code{Nightfall()} on the \code{WoodenHorse} object, will restore
#'   the input given to \code{GreekSoldier} to an S4 object.
#'
#'   Calling \code{HelenOfTroy()} on the \code{WoodenHorse} object returns the
#'   class of the \code{GreekSoldier} attribute.
#'
#'   Encoding the \code{WoodenHorse} object as a \code{vctrs_vctr} object allows
#'   us to pass the \code{rlang::is_vector} check and gives us nice options to
#'   preserve attributes over subsetting and concatenation operations.
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

setOldClass("WoodenHorse")

# Greek Soldier -----------------------------------------------------------

#' @rdname TheFallOfTroy
#' @export
setGeneric(
  "GreekSoldier",
  function(x) {
    standardGeneric("GreekSoldier")
  }
)

# By default, don't disguise as WoodenHorse
setMethod(
  "GreekSoldier",
  signature = c(x = "ANY"),
  function(x) x
)

# If inherits from Vector, disguise as WoodenHorse
setMethod(
  "GreekSoldier",
  signature = c(x = "Vector"),
  definition = function(x){
    new_vctr(
      c(0, NA_real_)[as.vector(is.na(x)) + 1],
      # numeric(length(x)),
      class = "WoodenHorse",
      GreekSoldier = x
    )
  }
)

setMethod(
  "GreekSoldier",
  signature = c(x = "AtomicList"),
  definition = function(x) {
    nas <- vapply(x, function(x){any(is.na(x))}, logical(1))
    new_vctr(
      c(0, NA_real_)[nas + 1],
      class = "WoodenHorse",
      GreekSoldier = x
    )
  }
)

# Night falls over Troy ---------------------------------------------------

#' @rdname TheFallOfTroy
#' @export
setGeneric(
  "Nightfall",
  function(x) {
    standardGeneric("Nightfall")
  }
)

# Just return input if not WoodenHorse
setMethod(
  "Nightfall",
  signature = c(x = "ANY"),
  function(x) x
)

# Return GreekSoldier if WoodenHorse
setMethod(
  "Nightfall",
  signature = c(x = "WoodenHorse"),
  function(x) attr(x, "GreekSoldier")
)

# Helen of Troy ----------------------------------------------------------

# Supposedly, Helen of Troy imitated the voices of the wives of the Greek
# soldiers in an attempt to reveal the contents of the Trojan Horse.

#' @rdname TheFallOfTroy
#' @export
setGeneric(
  "HelenOfTroy",
  function(x) {
    standardGeneric("HelenOfTroy")
  }
)

# Return regular class for any object
setMethod(
  "HelenOfTroy",
  signature = c(x = "ANY"),
  function(x) class(x)
)

# Return class of WoodenHorse's GreekSoldier attribute
setMethod(
  "HelenOfTroy",
  signature = c(x = "WoodenHorse"),
  function(x) {class(attr(x, "GreekSoldier"))}
)

# Wooden Horse boilerplate ------------------------------------------------

#' @importFrom vctrs new_vctr vec_data vec_assert
NULL

# Constructor, for internal use only
# External constructor is the GreekSoldier function.
.WoodenHorse <- function(x) {
  new_vctr(
    numeric(length(x)),
    GreekSoldier = x,
    class = "WoodenHorse"
  )
}

#' @method format WoodenHorse
#' @export
format.WoodenHorse <- function(x, ...) {
  ifelse(is.na(x), "NA", format(attr(x, "GreekSoldier")))
}

#' @importFrom vctrs obj_print_header
#' @method obj_print_header WoodenHorse
#' @export
obj_print_header.WoodenHorse <- function(x, ...) {
  cat(paste0("<", vec_ptype_full(x), ": ",
             classNameForDisplay(attr(x, "GreekSoldier")),
             "[", vec_size(x), "]>" ),
             "\n", collapse = "")
  invisible(x)
}

# Wooden Horse prototyping ------------------------------------------------

#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 WoodenHorse
#' @export
#' @export vec_ptype2.WoodenHorse
#' @noRd
#' @keywords internal
vec_ptype2.WoodenHorse <- function(x, y, ...) {
  UseMethod("vec_ptype2.WoodenHorse", y)
}

#' There is no common type between WoodenHorse specified classes
#' @method vec_ptype2.WoodenHorse default
#' @export
#' @noRd
#' @keywords internal
vec_ptype2.WoodenHorse.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' The common type between two WoodenHorse objects is a WoodenHorse object. If
#' the GreekSoldiers in x and y are not coercable into oneanother, we'll rely
#' on the default error signalling.
#' @method vec_ptype2.WoodenHorse WoodenHorse
#' @export
#' @noRd
#' @keywords internal
vec_ptype2.WoodenHorse.WoodenHorse <- function(x, y, ...) {
  new_vctr(
    numeric(0),
    GreekSoldier = bindROWS(
      attr(x, "GreekSoldier"),
      list(attr(y, "GreekSoldier")),
      use.names = FALSE, ignore.mcols = TRUE, check = FALSE),
    class = "WoodenHorse")
}

# Wooden Horse casting ----------------------------------------------------

#' @importFrom vctrs vec_cast
#' @method vec_cast WoodenHorse
#' @export
#' @export vec_cast.WoodenHorse
#' @noRd
#' @keywords internal
vec_cast.WoodenHorse <- function(x, to, ...) {
  UseMethod("vec_cast.WoodenHorse")
}

#' @method vec_cast.WoodenHorse default
#' @export
#' @noRd
#' @keywords internal
vec_cast.WoodenHorse.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}

#' @method vec_cast.WoodenHorse WoodenHorse
#' @export
#' @noRd
#' @keywords internal
vec_cast.WoodenHorse.WoodenHorse <- function(x, to, ...) {
  new_vctr(vec_data(x),
           GreekSoldier = attr(to, "GreekSoldier"),
           class = "WoodenHorse")
}

# Wooden Horse subsetting -------------------------------------------------

#' @method `[` WoodenHorse
#' @export
#' @noRd
#' @keywords internal
#' @importFrom vctrs vec_as_location
`[.WoodenHorse` <- function(x, i, ...) {
  ii <- vec_as_location(i, length(x), names = names(x), missing = "propagate")
  ii[is.na(i)] <- 1L
  new_vctr(vec_data(x)[i],
           GreekSoldier = attr(x, "GreekSoldier")[ii],
           class = "WoodenHorse")
}

#' @method `[[` WoodenHorse
#' @export
#' @noRd
#' @keywords internal
`[[.WoodenHorse` <- function(x, i, ...) {
  new_vctr(vec_data(x)[[i]],
           GreekSoldier = attr(x, "GreekSoldier")[[i]],
           class = "WoodenHorse")
}

# Subassignment -----------------------------------------------------------

#' @method `[<-` WoodenHorse
#' @export
#' @noRd
#' @keywords internal
`[<-.WoodenHorse` <- function(x, i, value) {
  if (inherits(value, "WoodenHorse")) {
    vec <- vec_data(value)
    value <- attr(value, "GreekSoldier")
  } else {
    vec <- numeric(length(value))
  }
  newvec <- `[<-`(vec_data(x), i = i, value = vec)
  # new <- replaceROWS(attr(x, "GreekSoldier"), i, value)
  new <- `[<-`(attr(x, "GreekSoldier"), i = i, value = value)
  new_vctr(
    newvec,
    GreekSoldier = new,
    class = "WoodenHorse"
  )
}

#' @method `[[<-` WoodenHorse
#' @export
#' @noRd
#' @keywords internal
`[[<-.WoodenHorse` <- function(x, i, value) {
  if (inherits(value, "WoodenHorse")) {
    value <- attr(value, "GreekSoldier")
  }
  vec <- 0
  newvec <- `[[<-`(vec_data(x), i = i, value = vec)
  new <- `[[<-`(attr(x, "GreekSoldier"), i = i, value = value)
  new_vctr(
    vec,
    GreekSoldier = new,
    class = "WoodenHorse"
  )
}


# Functions ---------------------------------------------------------------

#' @export
#' @method levels WoodenHorse
levels.WoodenHorse <- function(x) NULL

#' set NAs
#'
#' Not all S4 Vectors subclasses support having NAs.
#'
#' @param x An object to set NAs on
#' @param i A logical of \code{length(x)}, indicating which positions to set NAs.
#'
#' @return
#' @export
#'
#' @examples
#' NULL
setGeneric(
  "setNA",
  function(x, i) {
    standardGeneric("setNA")
  }
)

setMethod(
  "setNA",
  signature = c(x = "ANY"),
  definition = function(x, i) {
    vec_assert(i, logical(), size = length(x))
    x[i] <- NA
  }
)

setMethod(
  "setNA",
  signature = c(x = "WoodenHorse", i = "logical"),
  definition = function(x, i) {
    vec_assert(i, logical(), size = length(x))
    dat <- vec_data(x)
    dat[i] <- NA_real_
    new_vctr(
      dat,
      GreekSoldier = attr(x, "GreekSoldier"),
      class = "WoodenHorse"
    )
  }
)

# Mathy functions ---------------------------------------------------------

#' @importFrom vctrs vec_math
#' @method vec_math WoodenHorse
#' @export
#' @noRd
#' @keywords internal
vec_math.WoodenHorse <- function(.fn, .x, ...) {
  dat <- vec_data(.x)
  .x <- Nightfall(.x)
  .x <- plotmaths(x = .x, .fn, ...)
  if (inherits(.x, "Vector")) {
    .x <- new_vctr(
      dat[seq_along(.x)],
      GreekSoldier = .x,
      class = "WoodenHorse"
    )
  }
  return(.x)
}

#' @importFrom vctrs vec_arith
#' @method vec_arith WoodenHorse
#' @export
#' @export vec_arith.WoodenHorse
#' @noRd
#' @keywords internal
vec_arith.WoodenHorse <- function(op, x, y, ...) {
  UseMethod("vec_arith.WoodenHorse", y)
}

#' @export
#' @method vec_arith.WoodenHorse default
#' @noRd
#' @keywords internal
vec_arith.WoodenHorse.default <- function(op, x, y) {
  dat <- vec_data(x)
  x <- Nightfall(x)
  x <- plotarith(x, y, op)
  x <- new_vctr(
    dat,
    GreekSoldier = x,
    class = "WoodenHorse"
  )
}

#' @export
#' @method vec_arith.WoodenHorse MISSING
#' @noRd
#' @keywords internal
vec_arith.WoodenHorse.MISSING <- function(op, x, y) {
  dat <- vec_data(x)
  x <- Nightfall(x)
  x <- plotarith(x, op = op)
  if (inherits(x, "Vector")) {
    x <- new_vctr(
      dat,
      GreekSoldier = x,
      class = "WoodenHorse"
    )
  }
  return(x)
}

#' @export
#' @method vec_arith.WoodenHorse WoodenHorse
#' @noRd
#' @keywords internal
vec_arith.WoodenHorse.WoodenHorse <- function(op, x, y) {
  dat <- vec_arith_base(op, vec_data(x), vec_data(y))
  x <- Nightfall(x)
  y <- Nightfall(y)
  res <- plotarith(x, y, op)
  new_vctr(
    dat,
    GreekSoldier = res,
    class = "WoodenHorse"
  )
}
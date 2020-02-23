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
  definition = function(x){
    new_vctr(
      c(0, NA_real_)[as.vector(is.na(x)) + 1],
      # numeric(length(x)),
      class = "WoodenHorse",
      GreekSoldier = x
    )
  }
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

#' @describeIn TheFallOfTroy If the object is not a \code{WoodenHorse} return
#'   the input unaltered.
#' @usage NULL
setMethod(
  "Nightfall",
  signature = c(x = "ANY"),
  function(x) x
)

# Return GreekSoldier if WoodenHorse

#' @describeIn TheFallOfTroy If the object is a \code{WoodenHorse} return the
#'   \code{GreekSoldier} attribute within the \code{WoodenHorse}.
#' @usage NULL
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

#' @describeIn TheFallOfTroy Equivalent to calling \code{class(x)} for
#'   non-\code{WoodenHorse} objects.
#' @usage NULL
setMethod(
  "HelenOfTroy",
  signature = c(x = "ANY"),
  function(x) class(x)
)

#' @describeIn TheFallOfTroy Returns the \code{class} of the \code{GreekSoldier}
#'   attribute of the \code{WoodenHorse}.
#' @usage NULL
setMethod(
  "HelenOfTroy",
  signature = c(x = "WoodenHorse"),
  function(x) {class(attr(x, "GreekSoldier"))}
)

# Wooden Horse boilerplate ------------------------------------------------

#' @name WoodenHorse-vctr
#' @title WoodenHorse vctr methods
#'
#' @description Here are the methods listed for the \code{WoodenHorse} class.
#'   Refer to the documentation in the \code{vctrs} package to learn about their
#'   use.
#'
#' @details These are vctrs-methods that enable the \code{WoodenHorse} class,
#'   but should never be called directly by users. Consider these
#'   implementation details.
#'
#' @seealso See \code{\link[ggnomics]{TheFallOfTroy}} for the role that the
#'   \code{WoodenHorse} plays in the package.

#' @importFrom vctrs new_vctr vec_data vec_assert
NULL

#' @method format WoodenHorse
#' @export
format.WoodenHorse <- function(x, ...) {
  ifelse(is.na(x), "NA", format(attr(x, "GreekSoldier")))
}

#' @importFrom vctrs obj_print_header
#' @method obj_print_header WoodenHorse
#' @export
#' @describeIn WoodenHorse-vctr See \code{\link[vctrs]{obj_print_header}}
#' @usage NULL
obj_print_header.WoodenHorse <- function(x, ...) {
  cat(paste0("<", vec_ptype_full(x), ": ",
             classNameForDisplay(attr(x, "GreekSoldier")),
             "[", vec_size(x), "]>" ),
             "\n", collapse = "")
  invisible(x)
}

#' @importFrom vctrs vec_ptype_full
#' @method vec_ptype_full WoodenHorse
#' @export
#' @describeIn WoodenHorse-vctr See \code{\link[vctrs]{vec_ptype_full}}
#' @usage NULL
vec_ptype_full.WoodenHorse <- function(x, ...) {
  "WoodenHorse"
}

#' @importFrom vctrs vec_ptype_abbr
#' @method vec_ptype_abbr WoodenHorse
#' @export
#' @describeIn WoodenHorse-vctr See \code{\link[vctrs]{vec_ptype_abbr}}
#' @usage NULL
vec_ptype_abbr.WoodenHorse <- function(x, ...) {
  "WHrse"
}

# Wooden Horse prototyping ------------------------------------------------

#' @importFrom vctrs vec_ptype2
#' @method vec_ptype2 WoodenHorse
#' @export
#' @export vec_ptype2.WoodenHorse
#' @describeIn WoodenHorse-vctr Generic for \code{\link[vctrs]{vec_ptype2}}
#' @usage NULL
vec_ptype2.WoodenHorse <- function(x, y, ...) {
  UseMethod("vec_ptype2.WoodenHorse", y)
}

#' @method vec_ptype2.WoodenHorse default
#' @export
#' @describeIn WoodenHorse-vctr Default method for
#'   \code{vec_ptype2.WoodenHorse}: there are no common types.
#' @usage NULL
vec_ptype2.WoodenHorse.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

#' @method vec_ptype2.WoodenHorse WoodenHorse
#' @export
#' @describeIn WoodenHorse-vctr WoodenHorse method for
#'   \code{vec_ptype2.WoodenHorse}: attempt to merge
#'   \code{GreekSoldier} attributes.
#' @usage NULL
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
#' @describeIn WoodenHorse-vctr Generic for \code{\link[vctrs]{vec_cast}}
#' @usage NULL
vec_cast.WoodenHorse <- function(x, to, ...) {
  UseMethod("vec_cast.WoodenHorse")
}

#' @method vec_cast.WoodenHorse default
#' @export
#' @describeIn WoodenHorse-vctr Default method for \code{vec_cast.WoodenHorse}:
#'   it should not be casted.
#' @usage NULL
vec_cast.WoodenHorse.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}

#' @method vec_cast.WoodenHorse WoodenHorse
#' @export
#' @describeIn WoodenHorse-vctr \code{WoodenHorse} method for
#'   \code{vec_cast.WoodenHorse}: attempt to cast if the \code{GreekSoldier}
#'   attribute is similar.
#' @usage NULL
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

# Mathy functions ---------------------------------------------------------

#' @importFrom vctrs vec_math
#' @method vec_math WoodenHorse
#' @export
#' @describeIn WoodenHorse-vctr See \code{\link[vctrs]{vec_math}}. Exceptions to
#'   the math group generics for \code{WoodenHorse} class are \code{is.na()},
#'   \code{is.finite()} and \code{is.infinite()}.
#' @usage NULL
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
#' @describeIn WoodenHorse-vctr Generic for \code{\link[vctrs]{vec_arith}}
#' @usage NULL
vec_arith.WoodenHorse <- function(op, x, y, ...) {
  UseMethod("vec_arith.WoodenHorse", y)
}

#' @export
#' @method vec_arith.WoodenHorse default
#' @describeIn WoodenHorse-vctr Default method for \code{vec_arith.WoodenHorse}:
#'   try and apply arithmatic to the \code{GreekSoldier} attribute.
#' @usage NULL
vec_arith.WoodenHorse.default <- function(op, x, y, ...) {
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
#' @describeIn WoodenHorse-vctr Missing method for \code{vec_arith.WoodenHorse}:
#'   try and apply unary arithmatic to the \code{GreekSoldier} attribute.
#' @usage NULL
vec_arith.WoodenHorse.MISSING <- function(op, x, y, ...) {
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
#' @describeIn WoodenHorse-vctr \code{WoodenHorse} method for
#'   \code{vec_arith.WoodenHorse}: try and apply arithmatic between the
#'   \code{GreekSoldier} attribute of the two vectors.
#' @usage NULL
vec_arith.WoodenHorse.WoodenHorse <- function(op, x, y, ...) {
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

# is.finite / is.infinite and is.na are 'Math' group functions through the vctrs
# package. Since is.finite and is.infinite is ill-defined for most Vectors, we'd
# rather intercept them at the WoodenHorse class than in the vctrs class.
# Since the WoodenHorse is specialised for plotting and is.(in)finite is often
# used to decide wether data should be included, I see no obvious harm in this.

#' @method is.finite WoodenHorse
#' @export
#' @noRd
#' @keywords internal
is.finite.WoodenHorse <- function(x) {
  fun <- selectMethod("is.finite", HelenOfTroy(x))
  if (is.primitive(fun) || is.null(fun)) {
    return(rep(TRUE, length(x)) & is.finite(vec_data(x)))
  }
  ans <- fun(attr(x, "GreekSoldier")) & is.finite(vec_data(x))
  return(as.vector(ans))
}

#' @method is.infinite WoodenHorse
#' @export
#' @noRd
#' @keywords internal
is.infinite.WoodenHorse <- function(x) {
  fun <- selectMethod("is.infinite", HelenOfTroy(x))
  if (is.primitive(fun) || is.null(fun)) {
    return(rep(FALSE, length(x)) | is.infinite(vec_data(x)))
  }
  ans <- fun(attr(x, "GreekSoldier")) & is.infinite(vec_data(x))
  return(ans)
}

#' @method is.na WoodenHorse
#' @export
#' @noRd
#' @keywords internal
is.na.WoodenHorse <- function(x) {
  ans <- is.na(attr(x, "GreekSoldier"))
  ans <- as.vector(ans) | is.na(vec_data(x))
  return(ans)
}

# Functions ---------------------------------------------------------------

#' @export
#' @method levels WoodenHorse
levels.WoodenHorse <- function(x) NULL

#' @title set NAs
#'
#' @description Not all S4 Vectors subclasses support having NAs.
#'
#' @param x An object to set NAs on
#' @param i A logical of \code{length(x)}, indicating which positions to set NAs.
#'
#' @return The \code{x} argument with NAs set at positions where \code{i == TRUE}.
#' @noRd
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
    x
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
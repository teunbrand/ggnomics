setOldClass("WoodenHorse")
setOldClass("BeechHorse")
setOldClass("OakHorse")


# Constructors ------------------------------------------------------------

# BeechHorse is a simple wrapper around an S4 class, where the vec_data part
# keeps track of (assigned) NAs
BeechHorse <- function(x = MISSING()) {
  if (inherits(x, "DataFrame")) {
    nas <- is.na(x)
    nas <- pmin(rowSums(nas), 1)
  } else if (inherits(x, "CompressedList") || inherits(x, "SimpleList")) {
    nas <- is.na(x)
    nas <- vapply(nas, any, logical(1))
  } else {
    nas <- as.vector(is.na(x))
  }
  new_vctr(
    c(0, NA_real_)[nas + 1],
    GreekSoldier = x,
    class = c("BeechHorse", "WoodenHorse")
  )
}

# OakHorse is an index into an S4 class
OakHorse <- function(x = MISSING()) {
  new_vctr(
    seq_along(x),
    hsh = digest(x),
    GreekSoldier = x,
    class = c("OakHorse", "WoodenHorse")
  )
}

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

#' @method format BeechHorse
#' @export
format.BeechHorse <- function(x, ...) {
  ifelse(is.na(x), "NA", format(attr(x, "GreekSoldier")))
}

#' @method format OakHorse
#' @export
format.OakHorse <- function(x, ...) {
  d <- vec_data(x)
  ifelse(is.na(d), "NA", format(attr(x, "GreekSoldier"))[d])
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

#' @method vec_ptype2.WoodenHorse BeechHorse
#' @export
#' @describeIn WoodenHorse-vctr BeechHorse method for
#'   \code{vec_ptype2.WoodenHorse}: attempt to merge
#'   \code{GreekSoldier} attributes.
#' @usage NULL
vec_ptype2.WoodenHorse.BeechHorse <- function(x, y, ...) {
  new_vctr(
    numeric(0),
    GreekSoldier = bindROWS(
      attr(x, "GreekSoldier"),
      list(attr(y, "GreekSoldier")),
      use.names = FALSE, ignore.mcols = TRUE, check = FALSE),
    class = c("BeechHorse", "WoodenHorse"))
}

#' @method vec_ptype2.WoodenHorse OakHorse
#' @export
#' @describeIn WoodenHorse-vctr OakHorse method for
#'   \code{vec_ptype2.WoodenHorse}: only merge \code{GreekSoldier} attributes
#'   when necessary.
#' @usage NULL
vec_ptype2.WoodenHorse.OakHorse <- function(x, y, ...) {
  if (attr(x, "hsh") == attr(y, "hsh")) {
    return(y)
  } else if (length(y) == 0) {
    return(x)
  } else if (all(attr(x, "GreekSoldier") %in% attr(y, "GreekSoldier"))) {
    return(y)
  } else if (all(a <- attr(y, "GreekSoldier") %in% attr(x, "GreekSoldier"))) {
    return(x)
  } else {
    new <- bindROWS(
      attr(x, "GreekSoldier"),
      list(attr(y, "GreekSoldier")[!a]),
      use.names = FALSE, ignore.mcols = TRUE, check = FALSE
    )
    new_vctr(
      integer(0),
      hsh = digest(new),
      GreekSoldier = new,
      class = c("OakHorse", "WoodenHorse")
    )
  }
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

#' @method vec_cast.WoodenHorse BeechHorse
#' @export
#' @describeIn WoodenHorse-vctr \code{BeechHorse} method for
#'   \code{vec_cast.WoodenHorse}: attempt to cast if the \code{GreekSoldier}
#'   attribute is similar.
#' @usage NULL
vec_cast.WoodenHorse.BeechHorse <- function(x, to, ...) {
  new_vctr(vec_data(x),
           GreekSoldier = attr(to, "GreekSoldier"),
           class = c("BeechHorse", "WoodenHorse"))
}

#' @method vec_cast.WoodenHorse OakHorse
#' @export
#' @describeIn WoodenHorse-vctr \code{OakHorse} method for
#'   \code{vec_cast.WoodenHorse}: only refactor when hashes don't match
#' @usage NULL
vec_cast.WoodenHorse.OakHorse <- function(x, to, ...) {
  if (attr(x, "hsh") == attr(to, "hsh")) {
    return(x)
  } else {
    new <- match(attr(x, "GreekSoldier"), attr(to, "GreekSoldier"))
    new_vctr(
      new[vec_data(x)],
      hsh = attr(to, "hsh"),
      GreekSoldier = attr(to, "GreekSoldier"),
      class = c("OakHorse", "WoodenHorse")
    )
  }
}

# Wooden Horse subsetting -------------------------------------------------

#' @method `[` BeechHorse
#' @export
#' @noRd
#' @keywords internal
#' @importFrom vctrs vec_as_location
`[.BeechHorse` <- function(x, i, ...) {
  dat <- attr(x, "GreekSoldier")
  ii <- vec_as_location(i, NROW(dat), names = ROWNAMES(dat), missing = "propagate")
  ii[is.na(i)] <- 1L
  new_vctr(vec_data(x)[i],
           GreekSoldier = extractROWS(dat, i = ii),
           class = c("BeechHorse", "WoodenHorse"))
}

#' @method `[[` BeechHorse
#' @export
#' @noRd
#' @keywords internal
`[[.BeechHorse` <- function(x, i, ...) {
  new_vctr(vec_data(x)[[i]],
           GreekSoldier = attr(x, "GreekSoldier")[[i]],
           class = c("BeechHorse", "WoodenHorse"))
}

# Subassignment -----------------------------------------------------------

#' @method `[<-` BeechHorse
#' @export
#' @noRd
#' @keywords internal
`[<-.BeechHorse` <- function(x, i, value) {
  if (inherits(value, "WoodenHorse")) {
    vec <- vec_data(value)
    value <- attr(value, "GreekSoldier")
  } else {
    vec <- numeric(NROW(value))
  }
  newvec <- `[<-`(vec_data(x), i = i, value = vec)
  i[is.na(i)] <- 1L
  new <- mergeROWS(attr(x, "GreekSoldier"), i, value)
  # new <- `[<-`(attr(x, "GreekSoldier"), i = i, value = value)
  new_vctr(
    newvec,
    GreekSoldier = new,
    class = c("BeechHorse", "WoodenHorse")
  )
}

#' @method `[<-` OakHorse
#' @export
#' @noRd
#' @keywords internal
`[<-.OakHorse` <- function(x, i, value) {
  value <- GreekSoldier(value)
  ptype <- vec_ptype2(x, value)
  value <- vec_cast(value, ptype)
  x <- vec_cast(x, ptype)
  NextMethod()
}

#' @method `[[<-` BeechHorse
#' @export
#' @noRd
#' @keywords internal
`[[<-.BeechHorse` <- function(x, i, value) {
  vec <- c(0, NA_real_)[any(is.na(value)) + 1]
  if (inherits(value, "WoodenHorse")) {
    value <- Nightfall(value)
  }
  newvec <- `[[<-`(vec_data(x), i = i, value = vec)
  new <- `[[<-`(attr(x, "GreekSoldier"), i = i, value = value)
  new_vctr(
    vec,
    GreekSoldier = new,
    class = c("BeechHorse", "WoodenHorse")
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
  classx <- setdiff(class(.x), "vctrs_vctr")
  .x <- Nightfall(.x)
  .x <- plotmaths(x = .x, .fn, ...)
  if (inherits(.x, "Vector")) {
    .x <- new_vctr(
      dat[seq_along(.x)],
      GreekSoldier = .x,
      class = classx
    )
  }
  if (inherits(.x, "OakHorse")) {
    attr(.x, "hsh") <- digest(attr(.x, "GreekSoldier"))
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
  classx <- setdiff(class(x), "vctrs_vctr")
  x <- Nightfall(x)
  x <- plotarith(x, y, op)
  x <- new_vctr(
    dat,
    GreekSoldier = x,
    class = classx
  )
  if (inherits(x, "OakHorse")) {
    attr(x, "hsh") <- digest(attr(x, "GreekSoldier"))
  }
  x
}

#' @export
#' @method vec_arith.WoodenHorse MISSING
#' @noRd
#' @describeIn WoodenHorse-vctr Missing method for \code{vec_arith.WoodenHorse}:
#'   try and apply unary arithmatic to the \code{GreekSoldier} attribute.
#' @usage NULL
vec_arith.WoodenHorse.MISSING <- function(op, x, y, ...) {
  dat <- vec_data(x)
  classx <- setdiff(class(x), "vctrs_vctr")
  x <- Nightfall(x)
  x <- plotarith(x, op = op)
  if (inherits(x, "Vector")) {
    x <- new_vctr(
      dat,
      GreekSoldier = x,
      class = classx
    )
  }
  if (inherits(x, "OakHorse")) {
    attr(x, "hsh") <- digest(attr(x, "GreekSoldier"))
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
  dat <- vec_data(vec_cast(x, y))
  classx <- setdiff(class(x), "vctrs_vctr")
  x <- Nightfall(x)
  y <- Nightfall(y)
  res <- plotarith(x, y, op)
  x <- new_vctr(
    dat,
    GreekSoldier = res,
    class = classx
  )
  if (inherits(x, "OakHorse")) {
    attr(x, "hsh") <- digest(attr(x, "GreekSoldier"))
  }
  x
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

#' @method is.na BeechHorse
#' @export
#' @noRd
#' @keywords internal
is.na.BeechHorse <- function(x) {
  ans <- is.na(attr(x, "GreekSoldier"))
  ans <- as.vector(ans) | is.na(vec_data(x))
  return(ans)
}

is.na.OakHorse <- function(x) {
  d <- vec_data(x)
  ans <- is.na(attr(x, "GreekSoldier"))
  ans <- as.vector(ans)[d] | is.na(vec_data(x))
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
  signature = c(x = "BeechHorse", i = "logical"),
  definition = function(x, i) {
    vec_assert(i, logical(), size = length(x))
    dat <- vec_data(x)
    dat[i] <- NA_real_
    new_vctr(
      dat,
      GreekSoldier = attr(x, "GreekSoldier"),
      class = c("BeechHorse", "WoodenHorse")
    )
  }
)

setMethod(
  "setNA",
  signature = c(x = "OakHorse", i = "logical"),
  definition = function(x, i) {
    vec_assert(i, logical(), size = length(x))
    dat <- vec_data(x)
    dat[i] <- NA_integer_
    new_vctr(
      dat,
      hsh = attr(x, "hsh"),
      GreekSoldier = attr(x, "GreekSoldier"),
      class = c("OakHorse", "WoodenHorse")
    )
  }
)

# This probably doesn't belong here, but as long as S4 classes don't have format
# methods I'm keeping it.

#' @export
#' @noRd
#' @method format Vector
format.Vector <- function(x, ...) {
  showAsCell(x)
}

# Check finite ------------------------------------------------------------

# Should check wether data is finite like base::is.finite
setGeneric(
  "check_finite",
  function(x) standardGeneric("check_finite")
  
)

setMethod(
  "check_finite",
  signature = c(x = "ANY"),
  definition = function(x) is.finite(x)
)

# Since
setMethod(
  "check_finite",
  signature = c(x = "Vector"),
  definition = function(x) {
    fun <- selectMethod("is.finite", class(x))
    if (is.primitive(fun) || is.null(fun)) {
      valid <- validObject(x)
      return(rep(valid, length(x)))
    } else {
      return(is.finite(x))
    }
  }
)

# Miscellaneous -----------------------------------------------------------

# The rlang operator
`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

# Arithmatic --------------------------------------------------------------

#' Arithmatic for plots
#'
#' Arithmatic methods written for some classes can be a bit counterintuitive
#' when seen from a plotting perspective. This is a crude attempt at making some
#' plotting logic compatible with S4 classes hidden in WoodenHorses.
#'
#' @param x A left hand side argument.
#' @param y A right hand side argument.
#' @param op The name of the operator.
#'
#' @return Probably an object of the same type as the left hand side, who knows?
#'
#' @noRd
#'
#' @examples
#' require(GenomicRanges)
#'
#' # Compare the following
#' x <- GRanges(c("chr1:100-200", "chr2:200-300"))
#' y <- GreekSoldier(x)
#'
#' # In plotting logic we'd want to nudge the object 10 units to the right
#' x <- x + 10 # No nudging, expansion instead
#' y <- y + 10 # Proper nudge
setGeneric("plotarith", function(x, y, op) standardGeneric("plotarith"))

setMethod(
  "plotarith",
  signature = c("x" = "Vector"),
  function(x, y, op) {
    op <- getGeneric(op)
    return(op(x, y))
  }
)

#' @importFrom BiocGenerics start end
setMethod(
  "plotarith",
  signature = c("x" = "Ranges", "y" = "numeric"),
  function(x, y, op)  {
    fun <- getGeneric(op)
    m <- matrix(c(start(x), end(x)), ncol = 2)
    m <- fun(m, y)
    update_ranges(
      x,
      start = as.integer(pmin.int(m[, 1], m[, 2])),
      end = as.integer(pmax.int(m[, 1], m[, 2]))
    )
  }
)

setMethod(
  "plotarith",
  signature = c("x" = "Vector", "y" = "missing"),
  function(x, y, op) {
    switch(op, 
           "+" = x,
           "-" = plotarith(x, -1, "*"),
           stop("No arithmetic method know to do this operation."))
  }
)

# Mathematics -------------------------------------------------------------

# This is just to expose the internal Vector to the appropriate functions.

setGeneric("plotmaths", function(x, .fn, ...) standardGeneric("plotmaths"))

setMethod(
  "plotmaths",
  signature = c("x" = "Vector"),
  function(x, .fn, ...) {
    .fn <- getGeneric(.fn)
    return(.fn(x))
  }
)

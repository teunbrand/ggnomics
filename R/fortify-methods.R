# Documentation -----------------------------------------------------------

#' @name fortify-methods-S4
#'
#' @title Fortification of S4 vectors
#'
#' @description While the original \code{fortify()} function in ggplot2 was used
#'   to convert models and formulas to \code{data.frame}s, it can also be used
#'   as a hook to customise data prior to being put through the plotting
#'   internals. Since \code{ggplot} exclusively handles \code{data.frames}, we
#'   use \code{fortify} methods to convert S4 vector objects to base
#'   \code{data.frame}s.
#'
#' @details The fortify methods in this package are written for two classes.
#'   \enumerate{
#'    \item The \code{\link[S4Vectors]{Vector}} virtual class from which a broad
#'    range of concrete classes in Bioconductor derive. The fortify method
#'    column-binds the \code{Vector} content itself to the the result of
#'    callling \code{mcols()} on the Vector and returns this as a base
#'    \code{data.frame}. The column containing the \code{Vector} content will be
#'    named \code{.VectorClass}, wherein \code{VectorClass} is the name of the
#'    input's class.
#'    \item The \code{\link[S4Vectors]{DataFrame}} class is converted to its
#'    base R equivalent. Beware that any element metadata from the
#'    \code{Vector}s in the \code{DataFrame} is not converted to seperate
#'    columns.
#'   }
#'
#' @note Although S4 vectors can be converted to base R \code{data.frame}s,
#' there is no guarantee that all functions tailored to \code{data.frame} will
#' work. The \code{rbind} method for \code{data.frame} for example, implicitly
#' converts columns to vectors. When there exists no such method for an S4
#' class, \code{rbind} will throw an error.
#'
#' @return A base R \code{data.frame} with (when applicable) S4 columns
#'
#' @seealso \code{\link[ggplot2]{fortify}} for the original function in ggplot2.
#'
#' @examples
#' # Element metadata of Vector classes are converted to columns
#' x <- Rle(1:5)
#' mcols(x) <- DataFrame(y = LETTERS[1:5])
#' fortify(x)
#'
#' # Vector element metadata is not converted to a column when
#' # fortifying a DataFrame
#' df <- DataFrame(x = Rle(1:5), y = Factor(LETTERS[1:5]))
#' mcols(df$x) <- DataFrame(z = "I'm Vector metadata")
#' fortify(df)

# Vectors -----------------------------------------------------------------

#' @method fortify Vector
#' @export
#' @rdname fortify_S4
fortify.Vector <- function(model, data, ...) {
  m <- as.list(mcols(model))
  mcols(model) <- NULL
  name <- paste0(".", class(model))
  out <- setNames(c(list(model), m), c(name, names(m)))
  class(out) <- "data.frame"
  attr(out, "row.names") <- .set_row_names(length(model))
  out
}

# DataFrame ---------------------------------------------------------------

#' @method fortify DataFrame
#' @export
#' @rdname fortify_S4
fortify.DataFrame <- function(model, data, ...) {
  model <- as.list(model)
  class(model) <- "data.frame"
  attr(model, "row.names") <- .set_row_names(max(lengths(model)))
  model
}

# #' @method fortify SummarizedExperiment
# #' @export
# #' @rdname fortify_S4
# fortify.SummarizedExperiment <- function(model, data, ...) {
#   model
# }

# Matrices ----------------------------------------------------------------

# fortify.matrix <- function(model, data, ...) {
#   dinm <- dimnames(model)
#   row <- as.vector(row(model))
#   col <- as.vector(col(model))
#   if (!is.null(dinm)) {
#     if (!is.null(dinm[[1L]])) {
#       row <- dinm[[1L]][row]
#     }
#     if (!is.null(dinm[[2L]])) {
#       col <- dinm[[2L]][col]
#     }
#   }
#   out <- list(row = row, col = col, value = as.vector(model))
#   class(out) <- "data.frame"
#   attr(out, "row.names") <- .set_row_names(length(row))
#   out
# }
#
# fortify.Matrix <- fortify.matrix
#
# fortify.sparseMatrix <- function(model, data, ...) {
#   dinm <- dimnames(model)
#   model <- as(model, "TsparseMatrix")
#   row <- model@i + 1L
#   col <- model@j + 1L
#   if (!is.null(dinm[[1L]])) {
#     row <- dinm[[1L]][row]
#   }
#   if (!is.null(dinm[[2L]])) {
#     col <- dinm[[2L]][col]
#   }
#   out <- list(row = row, col = col, value = model@x)
#   class(out) <- "data.frame"
#   attr(out, "row.names") <- .set_row_names(length(row))
#   out
# }
#
# # Array -------------------------------------------------------------------
#
# fortify.array <- function(model, data, ...) {
#   di <- dim(model)
#   if (length(di) == 2L) {
#     model <- as.matrix(model)
#     return(fortify(model))
#   }
#   dinm <- dimnames(model)
#   slices <- lapply(seq_along(di), function(i) {
#     as.vector(slice.index(model, i))
#   })
#   slices <- setNames(slices, paste0("dim", seq_along(di)))
#   if (!is.null(dinm)) {
#     slices <- mapply(function(slice, names) {
#       if (is.null(names)) {
#         return(slice)
#       } else {
#         return(names[slice])
#       }
#     }, slice = slices, names = dinm, SIMPLIFY = FALSE)
#   }
#   out <- c(slices, list(value = as.vector(model)))
#   class(out) <- "data.frame"
#   attr(out, "row.names") <- .set_row_names(prod(di))
#   out
# }
#
# s4df <- function(x = list(), n = NULL) {
#   if (length(x) != 0 && is.null(names(x))) {
#     stop("Elements must be named")
#   }
#   lengths <- vapply(x, length, integer(1))
#   if (is.null(n)) {
#     n <- if (length(x) == 0 || min(lengths) == 0)
#       0
#     else max(lengths)
#   }
#   for (i in seq_along(x)) {
#     if (lengths[i] == n)
#       next
#     if (lengths[i] != 1) {
#       stop("Eleemnts must equal the number of rows or 1")
#     }
#     x[[i]] <- rep(x[[i]], n)
#   }
#   class(x) <- "data.frame"
#   attr(x, "row.names") <- .set_row_names(n)
#   x
# }

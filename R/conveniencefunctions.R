#' Passing a subset of data to ggplot2 layers.
#'
#' This is a convenience function to allow layer objects, such as geoms, to take
#' a subset of the data in the main \code{ggplot()} call, without storing a
#' duplicate of the subset in the ggplot object.
#'
#' @param rowtest logical \code{expression} indicating which rows to keep.
#'
#' @return A function that takes a \code{data.frame} as argument and returns a
#'   row-subset of that \code{data.frame} according to \code{rowtest}
#' @export
#'
#' @details \code{ggsubset} is a wrapper around \code{subset.data.frame} where
#'   the \code{subset} argument is set to \code{rowtest}. Since the \code{data}
#'   argument in the \code{layer()} function can take a function with one
#'   argument, we can pass the function returned from \code{ggsubset} as that
#'   argument to subset the data by rows.
#'
#' @seealso See \code{\link[ggplot2]{layer}}, specifically the \code{data}
#'   argument.
#'   See \code{\link[base]{subset.data.frame}} for the internal
#'   function.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(data = ggsubset(Species == "setosa"))
ggsubset <- function(rowtest) {
  rowtest <- substitute(rowtest)
  function(x) subset.data.frame(x, eval(rowtest))
}


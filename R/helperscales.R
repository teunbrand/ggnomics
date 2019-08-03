#' Formatter for log-transformed values
#'
#' Formats values from 1e+06 notation to 10^6 notation.
#'
#' @param x A \code{numeric} vector to format.
#'
#' @return A list of calls for formatted expressions.
#'
#' Written as a convenience funtion to make scales of log-transformed plots easier on the eyes.
#'
#' @export
#'
#' @examples
#' y <- format_logtrans(10^c(1:3))
format_logtrans <- function(x){
  x <- log10(x)
  lapply(x, function(i) do.call("substitute", list(expr(10^.x), list(.x = as.name(i)))))
}

#' Formatter for genomic coordinates
#'
#' Formats values from \code{"1e+06"} notation to \code{"1 Mb"}.
#'
#' @param x A \code{numeric} vector to format
#'
#' @return A \code{character} vector of formatted values
#'
#' @details Written as a convenience function to make genomic coordinates more
#'   pleasing to see.
#'
#' @export
#'
#' @examples
#' format_genco(10^seq(0, 7, by = 1))
format_genco <- function(x) {
  cutoffs  <- 10^seq(0, 6, by = 3)
  prefixes <- c(" bp", " kb", " Mb")
  idx <- pmax(1, findInterval(x, cutoffs))
  paste0(x/cutoffs[idx], prefixes[idx])
}


#' Center limits
#'
#' This a function factory that allows the centering of scales around a certain
#' value while still including all values. Convenient for centering log2 fold
#' change limits around zero.
#'
#' @param around A \code{numeric} of length 1 indicating around which value to
#'   center the limits.
#'
#' @return A \code{function} that takes limits and returns expanded limits
#'   centered at the \code{around} argument.
#' @export
#'
#' @examples
#' center_limits(5)(c(3,8))
#'
#' g <- ggplot(iris,
#'             aes(Sepal.Width, Sepal.Length,
#'                 colour = log2(Petal.Width / Petal.Length))) +
#'   geom_point() +
#'   scale_colour_gradient2(limits = center_limits())
center_limits <- function(around = 0) {
  function(input) {
    c(-1, 1) * max(abs(input - around)) + around
  }
}

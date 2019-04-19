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

#' Formatter for megabasepairs
#'
#' Formats values from "1e+06" notation to "1 Mb"
#'
#' @param x A \code{numeric} vector to format.
#'
#' @return A character vector of formatted values.
#'
#' @details Written as convenience function to make chromosome scales easier to read.
#'
#' @export
#'
#' @examples
#' format_megabase(c(1:3)*1e7)
format_megabase <- function(x){paste0(x/1e6, " Mb")}

# format_basepairs <- function(x){
#   z <- format(x, scientific = FALSE, trim = TRUE)
#   n <- nchar(z)
#   z <- ifelse(n > 3, paste0(x/1e3, " kb"), z)
#   z <- ifelse(n > 6, paste0(x/1e6, " Mb"), z)
#   return(z)
# }


try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun , "`.\n",
       "Please install and try again.", call. = FALSE)
}

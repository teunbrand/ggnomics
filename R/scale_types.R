# These functions let ggplot know what scales to choose.
# These are S3 methods that dispatch on the type of aesthetic

#' @export
#' @method scale_type Vector
scale_type.Vector <- function(x) {
  "S4_continuous"
}

#' @export
#' @method scale_type WoodenHorse
scale_type.WoodenHorse <- function(x) {
  scale_type(Nightfall(x))
}

#' @export
#' @method scale_type ANYGenomic
scale_type.ANYGenomic <- function(x) {
  "genomic"
}

#' @export
#' @method scale_type Factor
scale_type.Factor <- function(x) {
  if (is(levels(x), "knownDiscretes")) {
    "S4_discrete"
  } else if (is(levels(x), "ANYGenomic")) {
    "genomic" 
  } else {
    "S4_continuous"
  }
}

#' @export
#' @method scale_type Rle
scale_type.Rle <- function(x) {
  if (is(runValue(x), "knownDiscretes")) {
    "S4_discrete"
  } else {
    "S4_continuous"
  }
}

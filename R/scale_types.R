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

# For Factor dispatch on the levels
# Makes GRangesFactor become genomic.
#' @export
#' @method scale_type Factor
scale_type.Factor <- function(x) {
  scale_type(levels(x))
}

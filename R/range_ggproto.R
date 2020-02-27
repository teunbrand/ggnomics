# Constructors ------------------------------------------------------------

# The range ggproto is a class to keep track of data ranges. It stores the
# data range in the `range`-element and updates this with the `train`-element

new_S4_continuous_range <- function(aes) {
  ggproto(NULL, RangeS4Continuous, aes = aes)
}


new_S4_discrete_range <- function(aes) {
  ggproto(NULL, RangeS4Discrete, aes = aes)
}

# Range ggproto -----------------------------------------------------------

RangeS4 <- ggproto(
  "RangeS4", NULL,
  aes = NULL,
  range = NULL,
  reset = function(self) {
    self$range <- NULL
  }
)

RangeS4Continuous <- ggproto(
  "RangeS4Continuous",
  RangeS4,
  train = function(self, x) {
    self$range <- S4Train(new = x, existing = self$range,
                          aes = self$aes)
  }
)

RangeS4Discrete <- ggproto(
  "RangeS4Discrete",
  RangeS4,
  train = function(self, x, drop = FALSE, na.rm = FALSE) {
    self$range <- S4Train(new = x, existing = self$range,
                          drop = drop, na.rm = na.rm,
                          aes = self$aes)
  }
)

# Training ----------------------------------------------------------------

#' @name S4Train
#' @title Scale training
#'
#' @description Function for updating the scale range given a new and existing
#'   range.
#'
#' @param new An object representing the current range
#' @param existing An object representing the new range
#' @param aes An aesthetic for which to evaluate the range
#'
#' @return An updated representation of a range
#' @export
#'
#' @examples
#' # For plain numeric vectors
#' S4Train(new = 1:10, existing = c(-5, -15))
#'
#' # IRanges return plain limits
#' require(IRanges)
#' S4Train(new = IRanges("2501-2900"), existing = c(2000, 2500))
#'
#' # For GenomicRanges
#' require(GenomicRanges)
#' S4Train(new = GRanges(c("chr1:100-200", "chr2:1-2")),
#'         existing = GRanges(c("chr2:200-300")))
setGeneric(
  "S4Train",
  function(new, existing = NULL, ..., aes = "z") {
    if (is.null(new)) {
      return(existing)
    }
    standardGeneric("S4Train")
  },
  signature = c("new", "existing")
)

#' @rdname S4Train
setMethod(
  "S4Train",
  signature = c(new = "WoodenHorse"),
  definition = function(new, existing = NULL, aes = "z") {
    callGeneric(Nightfall(new)[!is.na(new)], Nightfall(existing), aes = aes)
  }
)

#' @rdname S4Train
setMethod(
  "S4Train",
  signature = c(new = "ANY", existing = "ANY"),
  definition = function(new, existing = NULL, aes = "z") {
    S4Range(new, existing, na.rm = TRUE, finite = TRUE, aes = aes)
  }
)

#' @rdname S4Train
setMethod(
  "S4Train",
  signature = c(new = "knownDiscretes", existing = "knownDiscretes_OR_missing"),
  definition = function(new, existing = NULL, drop = FALSE,
                        na.rm = FALSE, ..., aes = "z") {
    scales:::discrete_range(existing, new, drop = drop, na.rm = na.rm)
  }
)

#' @rdname S4Train
setMethod(
  "S4Train",
  signature = c(new = "IntegerRanges", existing = "numeric"),
  definition = function(new, existing = NULL, aes = "z") {
    new <- c(start(new), end(new))
    callGeneric(new, existing, aes = aes)
  }
)

# We suppress GRanges warning since if occurs that
# the seqlevels are not common between new and existing
#' @rdname S4Train
setMethod(
  "S4Train",
  signature = c(new = "GenomicRanges", existing = "GRanges"),
  definition = function(new, existing = NULL, aes = "z") {
    suppressWarnings(S4Range(new, existing, aes = aes))
  }
)
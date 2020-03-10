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

#' @describeIn ggnomics_extensions Identical to ggplot's Range ggproto. Note:
#'   this class and children are not exported and are for internal use.
#' @usage NULL
#' @format NULL
RangeS4 <- ggproto(
    "RangeS4", NULL,
    aes = NULL,
    range = NULL,
    reset = function(self) {
        self$range <- NULL
    }
)

#' @describeIn ggnomics_extensions A child to RangeS4 with S4 compatible train
#'   method.
#' @usage NULL
#' @format NULL
RangeS4Continuous <- ggproto(
    "RangeS4Continuous",
    RangeS4,
    train = function(self, x) {
        self$range <- S4Train(new = x,
                              existing = self$range,
                              aes = self$aes)
    }
)

#' @describeIn ggnomics_extensions A child to RangeS4 with S4 compatible train
#'   method.
#' @usage NULL
#' @format NULL
RangeS4Discrete <- ggproto(
    "RangeS4Discrete",
    RangeS4,
    train = function(self, x, drop = FALSE, na.rm = FALSE) {
        self$range <- S4Train(new = x,
                              existing = self$range,
                              drop = drop, na.rm = na.rm,
                              aes = self$aes)
    }
)

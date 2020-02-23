# S4 Labellers ------------------------------------------------------------

#' @name S4LabelFormat
#' @title Format labels for S4 classes
#' 
#' @description Attempts to figure out appropriate labels for S4 classes.
#'
#' @param x A vector to format
#' @param type Either \code{"major"} or \code{"minor"} to determine how to
#'   format the labels (default: \code{"major"}).
#' @param ... Optional arguments passed to downstream functions (not
#'   implemented).
#'
#' @details The \code{type} argument is currently only implemented for the
#'   \linkS4class{GenomicRanges} class.
#'
#' @return A \code{character} vector of the same length as \code{x} with labels
#' @export
#'
#' @examples
#' # Regular atomic vectors
#' S4LabelFormat(1:10)
#' S4LabelFormat(LETTERS[1:5])
#' 
#' require(GenomicRanges)
#' # GenomicRanges major labels are seqnames
#' S4LabelFormat(GPos("chr1", 1:10))
#' 
#' # GenomicRanges minor labels are positions formatted as basepairs
#' S4LabelFormat(GPos("chr1", 1:10), type = "minor")
setGeneric(
  "S4LabelFormat",
  function(x, type = "major", ...) standardGeneric("S4LabelFormat"),
  signature = "x"
)

#' @rdname S4LabelFormat
setMethod(
  "S4LabelFormat",
  signature(x = "ANY"),
  function(x, ...) {
    if (!is.null(names(x)))
      return(names(x))
    ret <- as.character(format(x, trim = TRUE, justify = "left"))
    ret[as.vector(is.na(x))] <- NA
    ret
  }
)

#' @rdname S4LabelFormat
setMethod(
  "S4LabelFormat",
  signature(x = "WoodenHorse"),
  function(x, ...) {
    callGeneric(Nightfall(x), type = type)
  }
)

#' @rdname S4LabelFormat
setMethod(
  "S4LabelFormat",
  signature(x = "ANYGenomic"),
  function(x, type = "major", ...) {
    x <- switch(type,
                "major" = as.character(seqnames(x)),
                "minor" = basepair_label(start(x)))
    callGeneric(x)
  }
)


# Basepair formatter ------------------------------------------------------

#' @name label_basepair
#' @title Label numbers as basepairs with SI suffix
#'
#' @description \code{basepair_label()} automatically scales and labels numbers
#'   with an appropriate SI suffix, e.g. "kb" for values \eqn{\ge} 1e3, "Mb" for
#'   \eqn{\ge} 1e6, "Gb" for \eqn{\ge} 1e9.
#'
#' @param x A vector to label. Can either be \code{numeric} of inherit from
#'   \linkS4class{GenomicRanges}, in which case takes the start positions to
#'   label.
#' @inheritParams scales::label_number_si
#' @param labelsmall A \code{logical} wether of not append the suffix to values
#'   smaller than 1000 (default: \code{FALSE}).
#'
#' @details The \code{label_basepair()} function is a function factory that
#'   returns a labelling function, i.e. a function that takes a vector \code{x}
#'   and returns a character vector of length(x) giving a label for each input
#'   value. The \code{basepair_label()} function is the default labeller from
#'   \code{label_basepair()} for convenience.
#'
#'   Note that these functions don't attempt to format numbers smaller than 1,
#'   as the units \emph{millibasepair} and \emph{centibasepairs} do not make
#'   sense.
#'
#'   When used in combination with
#'   \code{\link[=scale_x_genomic]{scale_(x|y)_genomic}}, these labelling
#'   functions work best as the \code{minor_labels} argument.
#'
#' @return A \code{function} for \code{label_basepair()} or a character vector
#'   of labels for \code{basepair_label()}.
#' @export
#'
#' @examples
#' require(scales)
#'
#' # The following labels are equivalent
#' demo_continuous(c(1, 1e9), label = label_basepair())
#' demo_continuous(c(1, 1e9), label = basepair_label)
#'
#' # When the label 'bp' is preffered over 'b'
#' demo_continuous(c(1, 1e9), label = label_basepair(unit = "bp"))
label_basepair <- function(accuracy = NULL, unit = "b", 
                           sep = NULL, labelsmall = FALSE, ...) {
  sep <- if (is.null(unit)) "" else " "
  # Force arguments
  force(accuracy)
  force(labelsmall)
  dots <- list(...)
  
  function(x) {
    if (missing(x) || is.null(x) || length(x) < 1L) {
      return(character(0))
    }
    
    breaks <- c(0, 10^c("k" = 3, "M" = 6, "G" = 9, "T" = 12))
    
    if (inherits(x, "WoodenHorse")) {
      x <- Nightfall(x)
    }
    if (inherits(x, "ANYGenomic")) {
      x <- start(x)
    }
    
    n_suffix <- cut(abs(x),
                    breaks = c(unname(breaks), Inf),
                    labels = c(names(breaks)),
                    right = FALSE)
    n_suffix[is.na(n_suffix)] <- ""
    
    if (labelsmall) {
      suffix <- paste0(sep, n_suffix, unit)
    } else {
      # Don't suffix basepairs unless >= kb
      suffix <- ifelse(nzchar(as.character(n_suffix)),
                       paste0(sep, n_suffix, unit), "")
    }

    scale <- 1 / breaks[n_suffix]
    scale[which(scale %in% c(Inf, NA))] <- 1
    
    if (is.null(accuracy)) {
      accuracy <- est_accuracy(x * scale)
    }
    
    args <- c(list(x = x, accuracy = accuracy, scale = unname(scale),
                   suffix = suffix), dots)
    
    do.call(scales::number, args)
    # scales::number(
    #   x,
    #   accuracy = accuracy,
    #   scale = unname(scale),
    #   suffix = suffix,
    #   ...
    # )
  }
}

#' @export
#' @rdname label_basepair
basepair_label <- label_basepair()

# Estimating the needed accuracy
# Mainly to get around r-lib/scales#251 issue
est_accuracy <- function(x) {
  if (all(!is.finite(x)) || length(x) == 1) {
    return(1)
  }
  small_diff <- min(diff(sort(unique(x))), 0)
  
  
  if (small_diff < sqrt(.Machine$double.eps) | !is.finite(small_diff)) {
    1
  } else {
    pmin(10^(floor(log10(small_diff))), 1)
  }
}

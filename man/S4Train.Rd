% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/range_training.R
\name{S4Train}
\alias{S4Train}
\alias{S4Train,NULL,ANY-method}
\alias{S4Train,WoodenHorse,ANY-method}
\alias{S4Train,ANY,ANY-method}
\alias{S4Train,knownDiscretes,knownDiscretes_OR_missing-method}
\alias{S4Train,IntegerRanges,numeric_OR_missing-method}
\alias{S4Train,GenomicRanges,GRanges_OR_missing-method}
\alias{S4Train,Rle,ANY-method}
\alias{S4Train,Factor,ANY-method}
\title{Scale training}
\usage{
S4Train(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{`NULL`,ANY}(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{WoodenHorse,ANY}(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{ANY,ANY}(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{knownDiscretes,knownDiscretes_OR_missing}(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{IntegerRanges,numeric_OR_missing}(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{GenomicRanges,GRanges_OR_missing}(new, existing = NULL, drop = FALSE, na.rm = FALSE, ..., aes = "z")

\S4method{S4Train}{Rle,ANY}(new, existing = NULL, drop = FALSE, na.rm = FALSE, aes = "z")

\S4method{S4Train}{Factor,ANY}(new, existing = NULL, drop = FALSE, na.rm = FALSE, aes = "z")
}
\arguments{
\item{new}{An object representing the current range}

\item{existing}{An object representing the new range}

\item{drop}{When \code{TRUE}, will drop factor levels not associated with
data. Used in discrete scales.}

\item{na.rm}{When \code{TRUE}, will remove missing values. Used in discrete
scales.}

\item{...}{Arguments passed down to downstream methods. Currently used for
discrete scales.}

\item{aes}{An aesthetic for which to evaluate the range}
}
\value{
An updated representation of a range
}
\description{
Function for updating the scale range given a new and existing
  range.
}
\examples{
# For plain numeric vectors
S4Train(new = 1:10, existing = c(-5, -15))

# IRanges return plain limits
require(IRanges)
S4Train(new = IRanges("2501-2900"), existing = c(2000, 2500))

# For GenomicRanges
require(GenomicRanges)
S4Train(new = GRanges(c("chr1:100-200", "chr2:1-2")),
        existing = GRanges(c("chr2:200-300")))
}

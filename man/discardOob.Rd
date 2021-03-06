% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/out_of_bounds.R
\name{discardOob}
\alias{discardOob}
\alias{discardOob,ANY,missing-method}
\alias{discardOob,numeric,numeric-method}
\alias{discardOob,GenomicRanges,GenomicRanges-method}
\alias{discardOob,WoodenHorse,ANY-method}
\title{Discard out of bounds values}
\usage{
discardOob(x, range = NULL, aes = "z")

\S4method{discardOob}{ANY,missing}(x, range = NULL, aes = "z")

\S4method{discardOob}{numeric,numeric}(x, range = c(0, 1), aes = "z")

\S4method{discardOob}{GenomicRanges,GenomicRanges}(x, range = GRanges(NA_character_, IRanges(0, 1)), aes = "z")

\S4method{discardOob}{WoodenHorse,ANY}(x, range = c(0, 1), aes = "z")
}
\arguments{
\item{x}{A vector of values to manipulate.}

\item{range}{A range representing object.}

\item{aes}{An aesthetic for which to evaluate the function.}
}
\value{
The values in \code{x} but with out of bounds values discarded.
}
\description{
Typically calls \code{\link[ggnomics]{censorThis}} on the
  values and subsequently discards \code{NA} values.
}
\examples{
# Regular numeric vectors
discardOob(c(0:5), c(2, 4))
}

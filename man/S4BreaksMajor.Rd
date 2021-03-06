% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breaks-major.R
\name{S4BreaksMajor}
\alias{S4BreaksMajor}
\alias{S4BreaksMajor,numeric-method}
\alias{S4BreaksMajor,GRanges-method}
\title{Major breaks for Vectors}
\usage{
S4BreaksMajor(x, n = 5L, ...)

\S4method{S4BreaksMajor}{numeric}(x, n = 5L, ...)

\S4method{S4BreaksMajor}{GRanges}(x, n = 5L, ...)
}
\arguments{
\item{x}{A object describing the range of values}

\item{n}{A desired number of breaks}

\item{...}{Arguments passed to downstream functions}
}
\value{
A vector with major breaks positions
}
\description{
Produces major breaks for use on plot axes.
}
\details{
The downstream function that arguments are typically passed to is
the \code{\link[labeling]{extended}} function, except when ranges
are given as \code{\linkS4class{GenomicRanges}}.
}
\examples{
# For simple numerics
S4BreaksMajor(c(0, 12))

# Gives GPos extremes for GRanges
require(GenomicRanges)
S4BreaksMajor(GRanges(c("chr1:100-200", "chr2:140-260")))
}

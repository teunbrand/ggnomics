% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/breaks-minor.R
\name{S4BreaksMinor}
\alias{S4BreaksMinor}
\alias{S4BreaksMinor,numeric,ANY-method}
\alias{S4BreaksMinor,ANY,GRanges-method}
\title{Minor breaks for Vectors}
\usage{
S4BreaksMinor(b, limits, n = 2)

\S4method{S4BreaksMinor}{numeric,ANY}(b, limits, n = 2)

\S4method{S4BreaksMinor}{ANY,GRanges}(b, limits, n = 5)
}
\arguments{
\item{b}{The current major breaks}

\item{limits}{An object representing the limits of the breaks}

\item{n}{The desired number of breaks}
}
\value{
A vector with minor breaks positions
}
\description{
Produces minor breaks for use on plot axes.
}
\examples{
S4BreaksMinor(c(1,3,5), c(0, 6), 2)

require(GenomicRanges)
S4BreaksMinor(GRanges(),
              GRanges(c("chr1:100-200", "chr2:140-260", "chr3:311-403")),
              n = 5)
}

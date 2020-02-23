#' @name S4BreaksMinor
#' @title Minor breaks for Vectors
#' @description Produces minor breaks for use on plot axes.
#'
#' @param b The current major breaks
#' @param limits An object representing the limits of the breaks
#' @param n The desired number of breaks
#'
#' @return A vector with minor breaks positions
#' @export
#'
#' @examples
#' S4BreaksMinor(c(1,3,5), c(0, 6), 2)
#'
#' require(GenomicRanges)
#' S4BreaksMinor(GRanges(),
#'               GRanges(c("chr1:100-200", "chr2:140-260", "chr3:311-403")),
#'               n = 5)
setGeneric(
  "S4BreaksMinor",
  function(b, limits, n = 2) standardGeneric("S4BreaksMinor")
)

# Whenever the major breaks are numeric, minor breaks default to the 
# scales::regular_minor_breaks() method

#' @rdname S4BreaksMinor
setMethod(
  "S4BreaksMinor",
  signature(b = "numeric"),
  function(b, limits, n = 2) {
    b <- b[!is.na(b)]
    if (length(b) < 2)
      return()
    bd <- diff(b)[1]
    if (min(limits) < min(b))
      b <- c(b[1] - bd, b)
    if (max(limits) > max(b))
      b <- c(b, b[length(b)] + bd)
    seq_between <- function(a, b) {
      seq(a, b, length.out = n + 1)[-(n + 1)]
    }
    breaks <- unlist(Map(seq_between, b[-length(b)], b[-1]))
    breaks <- c(breaks, b[length(b)])
    breaks
  }
)

# What the GRanges minor breaks do, is to ignore major breaks altogether,
# then take the largest stretch in IRanges space and compute the
# major breaks algorithm on that.
#' @rdname S4BreaksMinor
setMethod(
  "S4BreaksMinor",
  signature(b = "ANY", limits = "GRanges"),
  definition = function(b, limits, n = 5) {
    # First, disregard sequences and take stretches
    sets <- reduce(ranges(limits), with.revmap = TRUE)
    rmap <- mcols(sets)$revmap
    # Label the largest of stretches
    ismax <- which.max(width(sets))
    # We don't consider 2.5 to be a nice number for integers,
    # so we'll adjust the Q
    br <- labeling::extended(start(sets)[ismax], end(sets)[ismax], n,
                             Q = c(1, 5, 2, 4, 3))
    bd <- diff(br)[1]
    s <- start(sets) %% bd
    e <- end(sets) %% bd
    seqs <- lapply(seq_along(sets), function(i) {
      seqr <- seq(0, width(sets)[i] - s[i], by = bd)
      seqr + (start(sets)[i] %/% bd) * bd
    })
    ii <- rep(seq_along(sets), lengths(rmap))
    lens <- lengths(seqs)
    br2 <- GPos(
      rep(decode(seqnames(limits))[unlist(rmap)],
          lens[ii]),
      unlist(seqs[ii])
    )
    GreekSoldier(br2)
  }
)

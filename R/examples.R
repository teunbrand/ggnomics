#' Example cytobands data
#'
#' Generates some toy cytoband data to plot ideograms with.
#'
#' @return A \code{data.frame} containing cytoband data for the first two
#'   chromosomes of the hg38 human genome.
#' @export
#'
#' @seealso \code{\link[ggnomics]{example_cytoband_colours}}
#'
#' @examples
#' cyto <- example_cytobands()
example_cytobands <- function() {

  chr1 <- c(0, 23, 53, 71, 91, 125, 159, 201, 236, 276, 299, 323, 343,
            396, 437, 463, 502, 556, 585, 608, 685, 693, 844, 879, 915, 943,
            993, 1018, 1067, 1112, 1155, 1172, 1204, 1217, 1234, 1251, 1432,
            1475, 1506, 1551, 1566, 1591, 1605, 1655, 1672, 1709, 1730, 1761,
            1803, 1858, 1908, 1938, 1987, 2071, 2113, 2144, 2239, 2244, 2268,
            2305, 2346, 2364, 2435, 2489.56422)
  chr2 <- c(0, 44, 69, 120, 165, 190, 238, 277, 298, 318, 363, 383, 415,
            475, 526, 547, 610, 639, 684, 713, 733, 748, 831, 918, 939, 960,
            1021, 1053, 1067, 1087, 1122, 1181, 1216, 1291, 1317, 1343, 1361,
            1415, 1434, 1479, 1490, 1496, 1540, 1589, 1629, 1689, 1771, 1797,
            1821, 1885, 1911, 1966, 2025, 2041, 2082, 2145, 2207, 2243, 2252,
            2301, 2347, 2364, 2421.93529)
  data.frame(
    chrom = factor(rep.int(1:2, c(63, 62)), labels = c("chr1", "chr2")),
    chromStart = c(head(chr1, -1),  head(chr2, -1)) * 1e5,
    chromEnd   = c(tail(chr1, - 1), tail(chr2, -1)) * 1e5,
    name = "",
    gieStain = factor(
      c(2, 4, 2, 4, 2, 5, 2, 4, 2, 4, 2, 4, 2, 4, 2, 6, 2, 5, 2, 5,
        2, 3, 2, 6, 2, 6, 2, 3, 2, 5, 2, 5, 2, 1, 1, 7, 2, 5, 2, 5, 2,
        5, 2, 5, 2, 6, 2, 5, 2, 3, 2, 3, 2, 4, 2, 3, 2, 4, 2, 5, 2, 6,
        2, 2, 5, 2, 6, 2, 6, 2, 4, 2, 6, 2, 5, 2, 3, 2, 3, 2, 5, 2, 5,
        2, 3, 2, 1, 1, 2, 5, 2, 4, 2, 5, 2, 5, 2, 4, 2, 3, 2, 3, 2, 4,
        2, 6, 2, 6, 2, 5, 2, 6, 2, 6, 2, 5, 2, 3, 2, 6, 2, 3, 2, 5, 2),
      labels = c("acen", "gneg", "gpos100", "gpos25", "gpos50", "gpos75", "gvar")
    ),
    stringsAsFactors = FALSE
  )
}


#' Example colours for cytobands.
#'
#' Gives a selection of colours compatible with the \code{gieStain} column of
#' \code{example_cytobands}.
#'
#' @return A character vector of colours named by \code{gieStain} levels.
#' @export
#'
#' @seealso \code{\link[ggnomics]{example_cytobands}}
#' @examples
#' cytocols <- example_cytoband_colours()
example_cytoband_colours <- function() {
  setNames(c("#E41A1C", "grey90", "black", "grey65",
             "grey50",  "grey25", "grey1", "grey51"),
           c("acen",   "gneg",   "gpos100", "gpos25",
             "gpos50", "gpos75", "gvar",    "stalk"))
}

#' Generate fake Hi-C experiment
#'
#' Randomly generate data in the format of a GENOVA Hi-C experiment.
#'
#' @return A \code{list} with the skeleton of a GENOVA Hi-C experiment.
#'
#' @details The data is formatted as the first 120 Mb of 'chr1' at 1 Mb
#'   resolution. Since data is randomly generated, use \code{set.seed()} for
#'   reproducibility. Qualifies as the bare minimum requirement for the
#'   \code{\link[ggnomics]{geom_hictriangle}}'s \code{exp} argument.
#'
#' @export
#'
#' @examples
#' exp <- example_HiC()
example_HiC <- function() {
  try_require("MASS", "example_HiC")
  try_require("reshape2", "example_HiC")
  try_require("data.table", "example_HiC")
  ev <- cbind(cumsum(rt(120, 3)),
              cumsum(rt(120, 3)),
              cumsum(rt(120, 3)),
              cumsum(rt(120, 3)))
  ev <- ev %*% diag(c(300, 150, 75, 35)) %*% MASS::ginv(ev)
  ev <- ev + abs(min(ev))
  ev <- ev^2

  # Add a diagonal
  max <- max(ev)
  for(i in seq.int(0, ncol(ev) - 1, by = 1)) {
    test <- row(ev)  + i == col(ev)
    ev[test] <- (ev[test] + sqrt(dcauchy(i, 0, 5)) * max * 3)
  }
  ev <- ev - min(ev)

  # Format as symmetric matrix in triplet form as data.table
  ev[lower.tri(ev)] <- NA
  ev <- reshape2::melt(ev)
  ev <- ev[!is.na(ev$value),]
  rownames(ev) <- NULL
  ev <- data.table::data.table(
    V1 = ev$Var1,
    V2 = ev$Var2,
    V3 = ev$value
  )
  data.table::setkey(ev, "V1", "V2")

  # Setup genomic coordinates
  coords <- data.frame(V1 = "chr1",
                       V2 = as.integer(seq.int(0, 119e6, by = 1e6)),
                       V3 = as.integer(seq.int(1e6, 120e6, by = 1e6)),
                       V4 = 1:120)
  exp <- list(ICE = ev,
              ABS = coords,
              RES = 1e6)
}

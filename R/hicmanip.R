#' Checks for validity of a Hi-C layer
#'
#' @param exp1,exp2 A GENOVA experiment object.
#' @param xranges,yranges A GRanges object. Alternatively,a \code{list} of at least length 3 with the
#'   following elements: \describe{\item{\code{character}}{ vector of chromosome
#'   names}\item{\code{integer}}{ vector of start
#'   positions}\item{\code{integer}}{ vector of end positions}}
#'
#' @keywords internal
#' @return A \code{logical} of length 1.
check_valid_hiclayer <- function(exp1, exp2, xranges, yranges){
  # Check for presence of ICE, ABS and RES
  if (!all(c("ABS", "ICE", "RES") %in% names(exp1))){
    warning("'exp1' can not be interpreted as GENOVA experiment object")
  }

  # Check compatability of exp2
  if (!is.null(exp2)){
    if (!is.null(yranges)) {
      warning("Two experiments can not be plotted assymmetrically. Please only use 'xranges', or a single experiment.", call. = FALSE)
      return(FALSE)
    }
    if (exp1$RES != exp2$RES) {
      warning("Resolutions of 'exp1' and 'exp2' are different.", call. = FALSE)
      return(FALSE)
    }
    if (any(exp1$ABS != exp2$ABS)) {
      warning("Indices of experiments do not match.", call. = FALSE)
      return(FALSE)
    }
  }

  # Check validity of xranges
  if (is.list(xranges)) {
    if (length(xranges) < 3) {
      warning("'xranges' are undefined. Please supply a GRanges object or\na list with chromosome names, start- and end-coordinates, in that order.")
      return(FALSE)
    }
    if (!is.character(xranges[[1]]) & !is.factor(xranges[[1]])) {
      warning("Please supply 'xranges' chromosome names as characters or factors.")
      return(FALSE)
    }
    if (!is.numeric(xranges[[2]]) | !is.numeric(xranges[[3]])) {
      warning("Please supply 'xranges' start- and end-coordinates as numeric.")
      return(FALSE)
    }
  }

  # Check validity of yranges
  if (!is.null(yranges)) {
    if (is.list(yranges)) {
      if (length(yranges) < 3) {
        warning("'yranges' are undefined. Please supply a GRanges object or\na list with chromosome names, start- and end-coordinates, in that order.")
        return(FALSE)
      }
      if (!is.character(yranges[[1]]) & !is.factor(yranges[[1]])) {
        warning("Please supply 'yranges' chromosome names as characters of factors.")
        return(FALSE)
      }
      if (!is.numeric(yranges[[2]]) | !is.numeric(yranges[[3]])) {
        warning("Please supply 'yranges' start- and end-coordinates as numeric.")
        return(FALSE)
      }
    }
  }

  return(TRUE)
}

#' Extractor function for Hi-C GENOVA experiment objects.
#'
#' @param exp1,exp2 A GENOVA experiment object.
#' @param xranges,yranges A GRanges object. Alternatively,a \code{list} of at least length 3 with the
#'   following elements: \describe{\item{\code{character}}{ vector of chromosome
#'   names}\item{\code{integer}}{ vector of start
#'   positions}\item{\code{integer}}{ vector of end positions}}
#' @param triangle A \code{logical} of length 1. If \code{TRUE}, only return half a Hi-C matrix.
#'
#' @return A formatted \code{data.frame}
#'
#' @keywords internal
extract_hicdata <- function(exp1, exp2 = NULL, xranges, yranges = NULL, triangle = FALSE){
  try_require("GenomicRanges", "extract_hicdata")
  try_require("S4Vectors", "extract_hicdata")
  try_require("IRanges", "extract_hicdata")

  # Convert indices to GRanges
  gr <- GenomicRanges::GRanges(exp1$ABS[,1],
                               IRanges::IRanges(exp1$ABS[,2], exp1$ABS[,3]),
                               index = exp1$ABS[,4])

  # Check ranges
  if(is.list(xranges)){
    xranges <- GenomicRanges::GRanges(xranges[[1]], IRanges::IRanges(xranges[[2]], xranges[[3]]))
  }
  if(is.null(yranges)){
    yranges <- xranges
  } else {
    if(is.list(yranges)){
      yranges <- GenomicRanges::GRanges(yranges[[1]], IRanges::IRanges(yranges[[2]], yranges[[3]]))
    }
  }

  # Convert ranges to indices
  xidx <- gr$index[S4Vectors::to(GenomicRanges::findOverlaps(xranges, gr))]
  yidx <- gr$index[S4Vectors::to(GenomicRanges::findOverlaps(yranges, gr))]

  # Grab data
  if (triangle | !is.null(exp2)){
    data1 <- as.data.frame(exp1$ICE[expand.grid(xidx, yidx, KEEP.OUT.ATTRS = FALSE)])
    data1 <- data1[!is.na(data1[, 3]), ]
    if (triangle) {
      data2 <- data.frame(V1 = integer(0), V2 = integer(0), V3 = numeric(0))
    } else {
      data2 <- as.data.frame(exp2$ICE[expand.grid(xidx, yidx, KEEP.OUT.ATTRS = FALSE)])
      data2 <- data2[!is.na(data2[, 3]), ]
    }
  } else {
    data1 <- as.data.frame(exp1$ICE[expand.grid(xidx, yidx, KEEP.OUT.ATTRS = FALSE)])
    data1 <- data1[!is.na(data1[, 3]), ]
    data2 <- as.data.frame(exp1$ICE[expand.grid(yidx, xidx, KEEP.OUT.ATTRS = FALSE)])
    data2 <- data2[!is.na(data2[, 3]), ]
  }

  # Format data
  out <- data.frame(x = c(data1[,1], data2[,2]),
                    y = c(data1[,2], data2[,1]),
                    contacts = c(data1[,3], data2[,3]))
  out <- out[out$x %in% xidx & out$y %in% yidx,]

  xmatch <- match(out$x, gr$index)
  ymatch <- match(out$y, gr$index)
  out$x  <- GenomicRanges::start(gr)[xmatch] + 0.5 * exp1$RES
  out$y  <- GenomicRanges::start(gr)[ymatch] + 0.5 * exp1$RES
  attr(out, "xchr") <- GenomeInfoDb::seqnames(gr)[xmatch]
  attr(out, "ychr") <- GenomeInfoDb::seqnames(gr)[ymatch]

  return(out)
}

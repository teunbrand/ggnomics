#' Extract Hi-C data from GENOVA contacts objects.
#'
#' A convenience function for extracting 3D genome contact information from
#' \pkg{GENOVA}'s \code{contacts} objects.
#' 
#' @param exp1,exp2 A GENOVA \code{contacts} object.
#' @param xrange,yrange A \code{GRanges} object. Alternatively, a three-column
#'   \code{data.frame} with the following column structure (column names not required):
#'   \describe{
#'     \item{chromosome}{a character vector noting the chromosome name}
#'     \item{start}{an integer vector indicating the start position of the genomic range}
#'     \item{end}{an integer vector indicating the end position of the genomic range}
#'   }

#' @details When the '\code{exp2}' argument is not \code{NULL}, the second
#'   experiment must be compatible with the first in having identical indices.
#'
#'   The '\code{yrange}' argument must either be \code{NULL}, describe a single
#'   genomic range, or an equal number of genomic ranges as the '\code{xrange}'
#'   argument. Furthermore, the '\code{yrange}' argument must be strictly
#'   identical or downstream to the '\code{xrange}' argument.


#' @return A \code{data.frame} with the following columns:
#'   \describe{
#'     \item{\code{xidx}}{The Hi-C bin index for the x-position}
#'     \item{\code{yinx}}{The Hi-C bin index for the y-position}
#'     \item{\code{contacts}}{A numerical value representing the contacts between \code{xidx} and \code{yidx}}
#'     \item{\code{entry}}{An index into the '\code{xrange}' argument.}
#'     \item{\code{exp}}{An index determining the origin of the data, either 1 for the '\code{exp1}' argument or 2 for the '\code{exp2}' argument.}
#'     \item{\code{x}}{The basepair position of the start of \code{xidx}}
#'     \item{\code{y}}{The basepair position of the start of \code{yidx}}
#'     \item{\code{xchr}}{The chromosome for \code{xidx}}
#'     \item{\code{ychr}}{The chromosome for \code{yidx}}
#'   }
#' @export
#' 
#' @seealso For the \code{contacts} constructor: \code{\link[GENOVA]{load_contacts}}.
#'
#' @examples
#' x <- 1:10
hic_extractor <- function(exp1, exp2 = NULL, xrange = NULL, yrange = NULL) {
  try_require("data.table", "hic_extractor")
  
  # Checks compatibility of experiments
  if (!inherits(exp1, "contacts")) {
    stop("Please supply a GENOVA contacts object as 'exp1' argument",
         call. = FALSE)
  }
  if (!is.null(exp2)) {
    if (!inherits(exp2, "contacts")) {
      stop("Please supply a GENOVA contacts object as 'exp2' argument", 
           call. = FALSE)
    }
    if (!identical(attr(exp1, "resolution"), attr(exp2, "resolution"))) {
      stop("Resolutions of experiments are incompatible.", 
           call. = FALSE)
    }
    if (is.character(all.equal(exp1$IDX, exp2$IDX))) {
      stop("Indices of experiments are not equal. Cannot extract two ",
           "experiments with differences in indices reliably.",
           call. = FALSE)
    }
    explist <- list(exp1, exp2)
  } else {
    explist <- list(exp1)
  }
  
  if (is.null(xrange)) {
    stop("Don't know what regions of the Hi-C matrix to take.",
         " Please supply a valid `xrange` argument.")
  }
  
  # Translate ranges to Hi-C indices
  idx <- copy(explist[[1]]$IDX)
  
  xrange <- cbind(bed2idx(idx, xrange, "start"),
                  bed2idx(idx, xrange, "end"))
  
  if (!is.null(yrange)) {
    yrange <- cbind(bed2idx(idx, yrange, "start"),
                    bed2idx(idx, yrange, "end"))
  } else {
    yrange <- xrange
  }
  
  # Check compatibility of ranges
  if (any(dim(yrange) != dim(xrange)) & dim(yrange)[1] > 1) {
    message(paste0("'xrange' and 'yrange' have incompatible dimensions. ",
                   "Continuing with the first 'yrange' entry."))
    yrange <- yrange[1, , drop = FALSE]
  }
  
  # Translate Hi-C indices to data.table joins
  if (dim(yrange)[1] == 1) {
    get <- lapply(seq_len(nrow(xrange)), function(i) {
      CJ(V1 = seq.int(xrange[i, 1], xrange[i, 2]), 
         V2 = seq.int(yrange[1, 1], yrange[1, 2]), 
         entry = i)
    })
  } else {
    get <- lapply(seq_len(nrow(xrange)), function(i) {
      CJ(V1 = seq.int(xrange[i, 1], xrange[i, 2]),
         V2 = seq.int(yrange[i, 1], yrange[i, 2]),
         entry = i)
    })
  }
  get <- rbindlist(get)
  
  # Only take upper triangular matrix
  get <- get[V2 >= V1, ]
  
  # Loop over experiments, get data
  data <- lapply(seq_along(explist), function(i) {
    explist[[i]]$MAT[get, ][, exp := i]
  })
  data <- rbindlist(data)
  data[["V3"]][is.na(data[["V3"]])] <- 0
  
  # Translate indices back to genomic coordinates
  setkey(idx, V4)
  data[["x"]] <- idx[data[["V1"]], V2]
  data[["y"]] <- idx[data[["V2"]], V2]
  data[["xchr"]] <- idx[data[["V1"]], V1]
  data[["ychr"]] <- idx[data[["V2"]], V1]
  names(data) <- c("xidx", "yidx", "contacts", "entry", 
                   "exp", "x", "y", "xchr", "ychr")
  
  # Sneaky trick for automatic appropriate axes
  class(data[["y"]]) <- c("hicint", class(data[["y"]]))
  class(data[["x"]]) <- c("hicint", class(data[["x"]]))
  as.data.frame(data)
}

#' Match bed-like entries to Hi-C bin indices
#'
#' Copied from GENOVA
#'
#' @param IDX The IDX-slot of a \code{contacts} object
#' @param bed A 3-column data.frame / GRanges object
#' @param mode A \code{character} of length 1 indicating what position of the
#'   \code{bed} argument to match with the indices. Possible values:
#'   \code{"center"}, \code{"start"} or \code{"end"}.
#'
#' @return An \code{integer} vector of length \code{nrow(bed)} and parallel to
#'   \code{bed} with indices to the Hi-C matrix.
#'
#' @keywords internal
bed2idx <- function(IDX, bed, mode = c("centre", "start", "end")) {
  if (!inherits(bed, "data.frame") | is.data.table(bed)) {
    bed <- as.data.frame(bed)
  }
  
  # American/British spelling
  mode <- gsub("center", "centre", mode)
  mode <- match.arg(mode, c("centre", "start", "end"))
  
  # Reformat bed depending on mode
  bed <- cbind.data.frame(
    V1 = bed[, 1],
    V2 = switch(mode,
                "centre" = (bed[, 2] + bed[, 3]) / 2,
                "start" = bed[, 2],
                "end" = bed[, 3]
    )
  )
  
  class(IDX) <- "data.frame"
  
  # Assign entries to shared chromosomes
  chroms <- intersect(IDX[, 1], bed[, 1])
  bed_group <- match(bed[, 1], chroms)
  IDX_group <- match(IDX[, 1], chroms)
  
  # Split by chromosome
  bed_chrom <- split(bed[, 2], bed_group)
  IDX_chrom <- split(IDX[, c(2, 4)], IDX_group)
  
  # Match bed entry to idx
  out <- mapply(function(i, j) {
    j[pmax(findInterval(i, j[, 1]), 1), 2]
  }, i = bed_chrom, j = IDX_chrom)
  unsplit(out, bed_group)
}
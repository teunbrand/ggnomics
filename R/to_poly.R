#' Covert an object to polygon data.frame
#'
#' @param object An object to convert to a polygon.
#' @param selection A subset indication of the object.
#'
#' @details At the moment, this function supports converting objects of the
#'   following classes: \code{\link[rtracklayer]{BigWigFile}},
#'   \code{\link[IRanges]{RleList}}, \code{\link[S4Vectors]{Rle}} and
#'   \code{\link[base]{rle}} to polygons. Note that the \code{BigWigFile}
#'   requires a \code{\link[GenomicRanges]{GRanges}} selection argument.
#'
#'   For list-like objects, group IDs are automatically assigned. For objects
#'   with a selection containing genomic coordinates, sequence names are
#'   included if there are multiple.
#'
#' @return A data.frame with a parameterised polygon.
#' @export
#'
#' @examples
#' x <- rle(rep(1:5, each = 5))
#' to_poly(x)
setGeneric("to_poly", function(object, selection) standardGeneric("to_poly"))

#' @rdname to_poly
setMethod(
  "to_poly",
  signature(object = "BigWigFile", selection = "GenomicRanges"),
  function(object, selection) {
    try_require("GenomicRanges", "to_poly")
    try_require("rtracklayer", "to_poly")
    object <- rtracklayer::import.bw(object, selection = selection, as = "RleList")
    to_poly(object, selection)
  }
)

#' @rdname to_poly
setMethod(
  "to_poly",
  signature(object = "RleList", selection = "GenomicRanges"),
  function(object, selection) {
    try_require("GenomicRanges", "to_poly")
    try_require("S4Vectors", "to_poly")
    object <- object[selection]
    object <- to_poly(object)
    object$x <- object$x + start(selection)[object$group] - 1
    object
    if (length(unique(seqnames(selection))) > 1) {
      object$seqname <- decode(seqnames(selection))[object$group]
    }
    if (length(unique(names(selection))) > 1) {
      object$rangename <- names(selection)[object$group]
    }
    object
  }
)

#' @rdname to_poly
setMethod(
  "to_poly",
  signature(object = "RleList", selection = "missing"),
  function(object) {
    try_require("S4Vectors", "to_poly")
    object <- lapply(object, to_poly)
    idx <- vapply(object, base::nrow, integer(1))
    object <- do.call(rbind.data.frame, c(object, make.row.names = FALSE))
    object$group <- base::rep.int(seq_along(idx), idx)
    if (length(unique(names(idx))) == length(idx)) {
      object$group <- factor(object$group, labels = names(idx))
    }
    object
  }
)

#' @rdname to_poly
setMethod(
  "to_poly",
  signature(object = "Rle", selection = "missing"),
  function(object) {
    try_require("S4Vectors", "to_poly")
    df <- data.frame(
      x = c(1, base::rbind(start(object), end(object)), length(object)),
      y = c(0, base::rbind(runValue(object), runValue(object)), 0)
    )
    df[!duplicated(df),]
  }
)

#' @rdname to_poly
setMethod(
  "to_poly",
  signature(object = "rle", selection = "missing"),
  function(object) {
    end <- cumsum(object$lengths)
    start <- end - object$lengths + 1
    df <- data.frame(
      x = c(1, base::rbind(start, end), tail(end, 1)),
      y = c(0, base::rbind(object$values, object$values), 0)
    )
    df[!duplicated(df),]
  }
)

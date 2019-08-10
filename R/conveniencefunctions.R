#' Passing a subset of data to ggplot2 layers.
#'
#' This is a convenience function to allow layer objects, such as geoms, to take
#' a subset of the data in the main \code{ggplot()} call, without storing a
#' duplicate of the subset in the ggplot object.
#'
#' @param rowtest logical \code{expression} indicating which rows to keep.
#' @param omit a \code{character} column name to exclude.
#'
#' @return A function that takes a \code{data.frame} as argument and returns a
#'   subset of that \code{data.frame} according to \code{rowtest}
#' @export
#'
#' @details \code{ggsubset} is a wrapper around \code{subset.data.frame} where
#'   the \code{subset} argument is set to \code{rowtest} and the \code{select}
#'   argument to \code{-omit}. Since the \code{data} argument in the
#'   \code{layer()} function can take a function with one argument, we can pass
#'   the function returned from \code{ggsubset} as that argument to subset the
#'   data by rows.
#'
#' @seealso See \code{\link[ggplot2]{layer}}, specifically the \code{data}
#'   argument. See \code{\link[base]{subset.data.frame}} for the internal
#'   function.
#'
#' @examples
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   geom_point(data = ggsubset(Species == "setosa"))
ggsubset <- function(rowtest = NULL, omit = NULL) {
  rowtest <- substitute(rowtest)
  if (is.null(rowtest)) {
    rowtest <- substitute(TRUE)
  }

  if (!is.null(substitute(omit))) {
    omit <- substitute(-omit)
  } else {
    omit <- TRUE
  }

  function(x) subset.data.frame(x, eval(rowtest), eval(omit))
}


#' Helpers for gene models
#'
#' @description When you just want to plot some gene models and don't want to
#'   dabble in the dark arts of data wrangling, it would be convenient if there
#'   simply are functions that do most of the work for you. These are some of
#'   those functions.
#'
#' @param gff_file A \code{character} of length 1 containing a filename to a
#'   \code{.gff} file.
#' @param txdb A \code{TxDb} object from the \pkg{GenomicFeatures} package.
#' @param roi A \code{GRanges} object specifying a region of interest.
#' @param group_by A \code{character} of length 1, either \code{"gene"} or
#'   \code{"transcript"} specifying what feature type you want to summarise
#'   exons over.
#'
#' @details These functions import all required and optional data for
#'   \code{\link[ggnomics]{geom_genemodel}}.
#'
#'   Considering that different \code{.gff} files and \code{TxDb} objects
#'   observe different standards for their particular gene- and
#'   transcripts-names and -IDs, it was not possible to generalise the
#'   gene/transcript id columns to a specific output format. Furthermore, there
#'   is no guarantee that a \code{.gff} file and \code{TxDb} object for the same
#'   build of the same organism will yield identical results.
#'
#'   Note that genes and transcripts are imported that have \emph{any} overlap
#'   with the region of interest, so if you are particular about your region you
#'   might consider setting x-limits on the plot.
#'
#' @note These functions were not tested exhaustively, please report bugs as an
#'   issue on the GitHub page.
#'
#' @return A \code{data.frame} containing a representation of an exon in each
#'   row originating from the region of interest. Contains the following
#'   columns:
#'
#'   \describe{
#'     \item{seqnames}{the chromosome name.}
#'     \item{start}{the start coordinate of the exon.}
#'     \item{end}{the end coordinate of the exon.}
#'     \item{strand}{the orientation of the gene.}
#'     \item{id}{either a \code{transcript_id} or \code{gene_id} depending on the \code{group_by} argument.}
#'     \item{type}{specifying wether the exon is a coding sequence (CDS) or untranslated region (UTR).}
#'  }
#'
#' @name genemodel_helpers
#' @rdname genemodel_helpers
#' @examples
#' roi <- GRanges("chr17", IRanges(63e6, 64e6))
#'
#' # For plottable_genemodel_from_txdb()
#' library(TxDb.Hsapiens.UCSC.hg38.knownGene)
#' txdb <- TxDb.Hsapiens.UCSC.hg38.knownGene
#'
#' df <- plottable_genemodel_from_txdb(txdb, roi, group_by = "gene")
#'
#' ggplot(df) +
#'   geom_genemodel(aes(
#'     xmin = start, xmax = end, group = gene_id,
#'     strand = strand, type = type,
#'     colour = strand, fill = strand)
#'   )
#'
#' # For plottable_genemodel_from_gff()
#' file <- "../some/path/to/dir/genes.gtf"
#'
#' df <- plottable_genemodel_from_gff(file, roi, group_by = "gene")
#'
#' ggplot(df) +
#'   geom_genemodel(aes(
#'     xmin = start, xmax = end, group = gene_id,
#'     strand = strand, type = type,
#'     colour = strand, fill = strand)
#'   )
NULL

#' @rdname genemodel_helpers
#' @export
plottable_genemodel_from_txdb <- function(txdb, roi, group_by = "gene") {
  try_require("AnnotationDbi", "plottable_genemodel_from_txdb")
  try_require("GenomeInfoDb", "plottable_genemodel_from_txdb")
  try_require("GenomicFeatures", "plottable_genemodel_from_txdb")
  try_require("GenomicRanges", "plottable_genemodel_from_txdb")
  try_require("S4Vectors", "plottable_genemodel_from_txdb")
  group_by <- match.arg(group_by, c("gene", "transcript"))

  tx <- GenomicFeatures::transcriptsByOverlaps(txdb, roi)
  GenomeInfoDb::seqlevels(txdb) <- as.character(GenomeInfoDb::seqnames(roi)[1])
  tx2gene <- suppressMessages(
    AnnotationDbi::mapIds(txdb, as.character(tx$tx_id),
           column = if (group_by == "gene") "GENEID" else "TXNAME",
           keytype = "TXID")
  )

  utr5 <- GenomicFeatures::fiveUTRsByTranscript(txdb)
  utr5 <- utr5[names(utr5) %in% tx$tx_id]
  utr5 <- GenomicRanges::reduce(utr5)
  utr5 <- unlist(utr5)
  S4Vectors::mcols(utr5) <- S4Vectors::DataFrame(tx_id = tx2gene[as.character(names(utr5))],
                                      type = "5putr")

  utr3 <- GenomicFeatures::threeUTRsByTranscript(txdb)
  utr3 <- utr3[names(utr3) %in% tx$tx_id]
  utr3 <- GenomicRanges::reduce(utr3)
  utr3 <- unlist(utr3)
  S4Vectors::mcols(utr3) <- S4Vectors::DataFrame(tx_id = tx2gene[as.character(names(utr3))],
                                      type = "3putr")

  cds <- GenomicFeatures::cdsBy(txdb, "tx")
  cds <- cds[names(cds) %in% tx$tx_id]
  cds <- GenomicRanges::reduce(cds)
  cds <- unlist(cds)
  S4Vectors::mcols(cds) <- S4Vectors::DataFrame(tx_id = tx2gene[as.character(names(cds))],
                          type = "cds")

  if (group_by == "transcript") {
    out <- c(utr5, cds, utr3)
    S4Vectors::mcols(out)[["tx_id"]] <- NULL
    out <- sort(out)
    names(out) <- NULL
    out <- as.data.frame(out)
    out$width <- NULL
    return(out)
  } else {
    dat <- list(utr5 = utr5, utr3 = utr3, cds = cds)
    dat <- lapply(names(dat), function(i) {
      gr <- dat[[i]]
      gr <- split(gr, gr$tx_id)
      if (!inherits(gr, "GRangesList")) {
        gr <- methods::as(gr, "GRangesList")
      }
      gr <- GenomicRanges::reduce(gr)
      gr <- unlist(gr)
      S4Vectors::mcols(gr) <- S4Vectors::DataFrame(gene_id = names(gr),
                                                   type = i)
      gr
    })
    dat <- c(dat[[1]], dat[[2]], dat[[3]])
    dat <- sort(dat)
    names(dat) <- NULL
    dat <- as.data.frame(dat)
    dat$width <- NULL
    return(dat)
  }
}

#' @rdname genemodel_helpers
#' @export
plottable_genemodel_from_gff <- function(gff_file, roi, group_by = "gene") {
  try_require("rtracklayer", "plottable_genemodel_from_txdb")
  try_require("GenomicRanges", "plottable_genemodel_from_txdb")
  try_require("S4Vectors", "plottable_genemodel_from_txdb")
  group_by <- match.arg(group_by, c("gene", "transcript"))
  group_col <- paste0(group_by, "_id")
  gr <- rtracklayer::import.gff(gff_file, which = roi,
                                colnames = c("type", group_col))

  data <- list(utr = gr[grepl("utr", gr$type, ignore.case = TRUE)],
               cds = gr[grepl("cds", gr$type, ignore.case = TRUE)])

  data <- lapply(data, function(dat) {
    type <- droplevels(dat$type[1])
    out <- GenomicRanges::split(dat, S4Vectors::mcols(dat)[[group_col]])
    if (!inherits(out, "GRangesList")) {
      out <- methods::as(out, "GRangesList")
    }
    out <- GenomicRanges::reduce(out)
    out <- unlist(out)
    S4Vectors::mcols(out)[[group_col]] <- names(out)
    out$type <- type
    return(out)
  })
  data <- methods::as(data, "GRangesList")
  data <- unlist(data)
  names(data) <- NULL
  data <- sort(data)
  data <- as.data.frame(data)
  data$width <- NULL
  return(data)
}

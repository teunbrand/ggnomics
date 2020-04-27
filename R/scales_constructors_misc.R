#' @title Scales for exon types.
#'
#' @description Displaying gene models take the convention that coding sequences
#'   (CDSs) are drawn thicker than untranslated regions (UTRs). This scale type
#'   maps names of exontypes to thickness values.
#'
#' @param ... Arguments passed on to the S4 discrete scale
#' @param palette A palette function that returns a named numerical vector that
#'   maps exon type names to widths.
#' @param aesthetics The names of the aesthetics that this scale works with.
#' @param guide A function used to create a guide or its name.
#'
#' @details This scale is typically used in conjunction with
#'   \code{\link[ggnomics]{geom_genemodel}}.
#'
#'   By default, the following
#'   characters are recognised as CDSs: \code{"CDS"}, \code{"cds"}, and are
#'   drawn at 1x thickness. The following characters are recognised as UTRs:
#'   \code{"UTR"}, \code{"utr"}, \code{"three_prime_UTR"},
#'   \code{"three_prime_utr"}, \code{"five_prime_UTR"}, \code{"five_prime_utr"},
#'   and are drawn at 0.5x thickness.
#'
#' @return A \code{ScaleS4} object.
#' @export
#'
#' @examples
#' NULL
scale_exontype_discrete <- function(
    ..., palette = NULL, aesthetics = "exontype", guide = "none"
) {
    if (is.null(palette)) {
        palette <- exontype_palette
    }
    S4_discrete_scale("exontype", aesthetics, palette = palette,
                      guide = guide, ...)
}

# Let me know if I have skipped some ways of common UTR/CDS notation
exontype_palette <- function(...) {
    c("CDS" = 1, "cds" = 1,
      "UTR" = 0.5, "utr" = 0.5,
      "three_prime_UTR" = 0.5, "three_prime_utr" = 0.5,
      "five_prime_UTR" = 0.5, "five_prime_utr" = 0.5)
}

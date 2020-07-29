# Generic -----------------------------------------------------------------

#' Setup ideograms
#'
#' Sets up a local cache of ideogram data for a genome.
#'
#' @param data One of the following:\itemize{
#'   \item{ A \code{data.frame} with the following 6 columns: \describe{
#'     \item{chrom}{A \code{character} column with chromosome names.}
#'     \item{chromStart}{An \code{integer} vector with band start positions.}
#'     \item{chromEnd}{An \code{integer} vector with band end positions.}
#'     \item{name}{A \code{character} with names for cytogenetic bands.}
#'     \item{gieStain}{A \code{character} with Giemsa stain results. Recognised
#'     values are: \code{"gneg"}, \code{"gpos"}, \code{"gpos50"},
#'     \code{"gpos75"}, \code{"gpos25"}, \code{"gpos100"}, \code{"acen"},
#'     \code{"gvar"}, and \code{"stalk"}.}}}
#'   \item{A \code{character} with the filename from which the data are to be
#'     read from.}
#'   \item{Can be \code{missing} if the \code{genome} argument is set. See
#'   details.}
#' }
#' @param colourmap A named \code{character} vector with colours where names
#'   correspond to the \code{gieStain} column.
#' @param genome A \code{character} with the genome name, e.g. \code{"mm10"}.
#'
#' @details If the \code{data} argument is missing but the \code{genome}
#'   argument is either \code{"hg19"}, \code{"hg38"}, \code{"mm9"} or
#'   \code{"mm10"}, BiocFileCache is used to cache the \code{cytoBand.txt}
#'   file from UCSC.
#'
#' @seealso This is to be used in conjunction with ideogram axes:
#'   \code{\link[ggnomics]{guide_ideogram_axis}()}
#'
#' @return Returns no value, but caches the ideograms under the \code{genome}
#'   identifier in a temporary cache.
#' @export
#'
#' @examples
#' setup_ideograms(genome = "hg38")
setGeneric(
    "setup_ideograms",
    function(data = NULL,
             genome = NULL,
             colourmap = default_ideogram_colours()) standardGeneric("setup_ideograms")
)

# Core --------------------------------------------------------------------

.setup_ideogram <- function(data, genome = NULL,
                            colourmap = default_ideogram_colours()
) {
    data <- split(data, data[, 1])
    nrows <- vapply(data, nrow, integer(1))
    data <- data[nrows > 1]

    ideograms <- lapply(data, function(ideo) {
        nbands <- nrow(ideo)
        centro <- which(ideo[, 5] == "acen")
        if (!length(centro)) {
            centro <- c(0,0)
        }

        outline <- c(ideo[c(1, centro[1]), 2],
                     ideo[c(centro[2], nbands), 3])
        if (!all(centro == 0)) {
            outline <- c(outline[1:2], mean(outline[2:3]), outline[3:4])
            outline <- data.frame(
                x = c(outline, rev(outline)),
                y = c(0, 0, 0.5, 0, 0, 1, 1, 0.5, 1, 1)
            )
        } else {
            outline <- data.frame(x = c(outline, rev(outline)),
                                  y = c(0, 0, 1, 1))
        }

        bands <- t(ideo[, 2:3])
        dim(bands) <- NULL
        bands <- data.frame(
            x = rep(bands, each = 2),
            y = c(0, 1, 1, 0),
            fill = rep(colourmap[as.character(ideo[, 5])], each = 4),
            id = rep(seq_len(nrow(ideo)), each = 4),
            stringsAsFactors = FALSE
        )
        return(list(outline = outline, bands = bands))
    })

    if (is.null(genome)) {
        genome <- "unknown"
    }

    assign(genome, ideograms, envir = ideo_cache)
    return(invisible())
}


# Methods -----------------------------------------------------------------

setMethod(
    "setup_ideograms",
    signature = c(data = "data.frame"),
    .setup_ideogram
)

setMethod(
    "setup_ideograms",
    signature = c(data = "character"),
    function(data, genome = NULL, colourmap = default_ideogram_colours()) {
        data <- read.table(data, sep = "\t", stringsAsFactors = FALSE)
        .setup_ideogram(data, colourmap  = colourmap, genome = genome)
    }
)

setMethod(
    "setup_ideograms",
    signature = c(data = "missing_OR_NULL", genome = "character"),
    function(data, genome = NULL,
             colourmap = default_ideogram_colours()) {
        try_require("BiocFileCache", "setup_ideograms")
        data <- genome
        if (is.null(genome)) {
            genome <- data
        }
        if (length(data) > 1) {
            stop("Please provide a single genome name as argument.",
                 call. = FALSE)
        }
        if (genome %in% ls(ideo_cache)) {
            stop("Ideograms already in cache.", call. = FALSE)
        }

        url <- switch(
            genome,
            "mm10" = "http://hgdownload.cse.ucsc.edu/goldenpath/mm10/database/cytoBand.txt.gz",
            "mm9"  = "http://hgdownload.cse.ucsc.edu/goldenpath/mm9/database/cytoBand.txt.gz",
            "hg38" = "http://hgdownload.cse.ucsc.edu/goldenpath/hg38/database/cytoBand.txt.gz",
            "hg19" = "http://hgdownload.cse.ucsc.edu/goldenpath/hg19/database/cytoBand.txt.gz",
            stop("Genome not implemented.", call. = FALSE)
        )
        bfc <- BiocFileCache::BiocFileCache()
        path <- BiocFileCache::bfcrpath(bfc, url)

        data <- read.table(path, sep = "\t", stringsAsFactors = FALSE)

        .setup_ideogram(data, colourmap  = colourmap, genome = genome)
    }
)

# Cache -------------------------------------------------------------------

ideo_cache <- new.env()

ideo_cache_get <- function(genome = NULL) {
    if (is.null(genome)) {
        genome <- "unknown"
    }
    if (!genome %in% ls(ideo_cache) && length(ideo_cache)) {
        # Try first ideogram cache
        genome <- ls(ideo_cache)[[1]]
    }
    if (genome %in% ls(ideo_cache)) {
        out <- get(genome, envir = ideo_cache)
        return(out)
    } else {
        stop("No cached ideograms found for genome `", genome, "`. Please run ",
             "`setup_ideograms()` first.", call. = FALSE)
    }
}

clear_ideogram_cache <- function() {

}

# Helpers -----------------------------------------------------------------

default_ideogram_colours <- function() {
    specials <- c(gneg = "white", stalk = "brown3", acen = "#E41A1C",
                  gpos = "black", gvar = "black")
    graypal <- scales::brewer_pal(palette = "Greys")(9)
    graypal <- scales::gradient_n_pal(graypal)(seq(0, 1, length.out = 100))
    names(graypal) <- paste0("gpos", seq_along(graypal))
    return(c(specials, graypal))
}

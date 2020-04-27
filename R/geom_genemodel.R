# Main function -----------------------------------------------------------
#' @include geom_range.R
NULL

#' Gene models
#'
#' @description \code{geom_genemodel} is a specialised geom for drawing gene
#'   models. It draws coding sequences thicker than untranslated regions and
#'   styles introns with arrows, lines or chevrons. By default, it separates
#'   different overlapping models along the y-axis.
#'
#' @inheritParams ggplot2::geom_rect
#' @param position Position adjustment, either as a string, or the result of a
#'   call to a position adjustment function.
#' @param intron.style By default, introns are displayed as a series of arrows
#'   (\code{"arrowline"}) when a \code{strand} aesthetic contains \code{"+"} or
#'   \code{"-"}. Alternatively, a \code{"chevron"} option is available. In
#'   absence of strand information, the intron style defaults to a simple line
#'   (\code{"line"}).
#' @param arrow Specification for arrow heads, as created by
#'   \code{\link[grid]{arrow}}. Only applicable when \code{intron.style =
#'   "arrowline"} (the default). Defaults internally to \code{grid::arrow(length
#'   = grid::unit(2, "mm"))}.
#' @param arrow.freq A \code{\link[grid]{unit}} object specifying the distance
#'   over which arrows should be repeated. Only applicable when
#'   \code{intron.style = "arrowline"}.
#' @param chevron.height A \code{numeric} of length one specifying how high
#'   chevrons should rise relative to coding sequences. Only applicable when
#'   \code{intron.style = "chevron"}.
#'
#' @details The expected input format is that every exon has its own row. The
#'   genomic location of the exons are to be given to the \code{xmin} aesthetic
#'   for start positions and the \code{xmax} aesthetic for the end position.
#'   Genes are interpreted to be groups of exons specified by the required
#'   \code{group} aesthetic. Exons that belong to the same group will be
#'   connected by introns.
#'
#'   Optionally, a distinction between coding sequences (CDSs) and untranslated
#'   regions (UTRs) can be made for each exon by providing the \code{exontype}
#'   aesthetic. Also, strand information can be used to style introns in the
#'   \code{"arrowline"} or \code{"chevron"} fashion by providing a \code{strand}
#'   aesthetic, which assumes that \code{"+"} should be oriented left-to-right
#'   and \code{"-"} should be right-to-left. A \code{y} aesthetic can be
#'   provided to offset the gene models from the x-axis. The \code{thickness}
#'   aesthetic controls how thick a CDS should be drawn in y-axis units.
#'
#'   Alternatively, this geom can also be used to visualise transcript models,
#'   in which case the following recommendations apply. Supply a transcript name
#'   or transcript ID to the group aesthetic instead of a gene name or gene ID.
#'   Set \code{intron.style = "chevron"} which is more appropriate for splice
#'   junctions.
#'
#' @section Aesthetics:
#'
#'   \code{geom_genemodel} understands the following aesthetics (required
#'   aesthetics are in bold, optional aesthetics in italics)
#'
#'   \itemize{
#'   \item{\strong{xmax}}
#'   \item{\strong{xmin}}
#'   \item{\strong{group}}
#'   \item{\emph{y}}
#'   \item{\emph{thickness}}
#'   \item{\emph{type}}
#'   \item{\emph{strand}}
#'   \item{colour}
#'   \item{fill}
#'   \item{size}
#'   \item{linetype}
#'   \item{alpha} }
#'
#' @seealso \code{\link[ggnomics]{position_disjoint_ranges}}
#'
#' @export
#'
#' @examples
#' # Using GRanges for exons
#' require(GenomicRanges)
#' gm <- example_genemodels()
#' df <- DataFrame(x = granges(gm), mcols(gm))
#'
#' ggplot(df) +
#'     geom_genemodel(aes(x = x, group = name, exontype = type))
geom_genemodel <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = position_disjoint_ranges(extend = 1e4),
    ...,
    intron.style = "arrowline",
    arrow = NULL,
    arrow.freq = unit(4, "mm"),
    chevron.height = 1,
    linejoin = "mitre",
    na.rm = FALSE,
    show.legend = NA,
    inherit.aes = TRUE
) {
    intron.style <- match.arg(
        intron.style,
        c("arrowline", "line", "chevron")
    )
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomGenemodel,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            intron.style = intron.style,
            chevron.height = chevron.height,
            arrow = arrow,
            arrow.freq = arrow.freq,
            linejoin = linejoin, na.rm = na.rm, ...
        )
    )
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggnomics_extensions
GeomGenemodel <- ggproto(
    "GeomGenemodel", GeomRange,
    default_aes = aes(colour = "grey35",
                      fill = "grey35",
                      size = 0.5,
                      linetype = 1,
                      alpha = NA,
                      y = 0,
                      thickness = 0.8,
                      strand = "*",
                      exontype = 1),
    required_aes = c("x|y", "group"),
    optional_aes = c("xend|yend", "strand", "exontype","thickness"),
    non_missing_aes = c("exontype"),
    setup_data = function(self, data, params) {
        if (!is.null(data[["strand"]])) {
            data[["strand"]] <- as.character(data[["strand"]])
        } else if (HelenOfTroy(data[["x"]], "ANYGenomic")) {
            data[["strand"]] <- as.character(strand(Nightfall(data[["x"]])))
        } else if (HelenOfTroy(data[["y"]], "ANYGenomic")) {
            data[["strand"]] <- as.character(strand(Nightfall(data[["y"]])))
        }
        ggproto_parent(GeomRange, self)$setup_data(data, params)
    },
    draw_panel = function(
        self, data, panel_params, coord,
        intron.style = "line", chevron.height = 1,
        arrow = NULL, arrow.freq = unit(4, "mm"),
        linejoin = "mitre"
    ) {
        if (HelenOfTroy(data$x, "Ranges")) {
            data <- transform(data, xmin = x, xmax = x, x = NULL)
        } else {
            data <- transform(data, xmin = x, xmax = xend,
                              x = NULL, xend = NULL)
        }
        if (HelenOfTroy(data$y, "Ranges")) {
            data <- transform(data, ymin = y, ymax = y, y = NULL)
        } else {
            data <- transform(data, y = (y + yend)/2)
            data <- transform(data, ymin = y - 0.5 * thickness * exontype,
                              ymax = y + 0.5 * thickness * exontype,
                              y = NULL, yend = NULL, exontype = NULL,
                              thickness = NULL)
        }

        coords <- coord$transform(data, panel_params)
        coords$colour <- scales::alpha(coords$colour, coords$alpha)
        coords$fill <- scales::alpha(coords$fill, coords$alpha)

        # Build exons
        exons <- grid::rectGrob(
            coords$xmin, coords$ymin,
            width  = coords$xmax - coords$xmin,
            height = coords$ymax - coords$ymin,
            default.units = "npc",
            just = c("left", "bottom"),
            gp = grid::gpar(
                col = coords$colour,
                fill = coords$fill,
                lwd = coords$size * .pt,
                lty = coords$linetype,
                linejoin = linejoin,
                lineend = if (identical(linejoin, "round")) "round" else "square"
            )
        )
        exons$name <- grid::grobName(exons, "exons")


        # Distribute intron styling
        if (intron.style == "line" || !any(c("+", "-") %in% coords$strand)) {
            introns <- style_intron_plainline(coords, linejoin)
            introns$name <- grid::grobName(introns, "introns")
            genemodel <- grid::gTree(children = grid::gList(introns, exons))
            genemodel$name <- grid::grobName(genemodel, "genemodel")
            return(genemodel)
        }

        if (intron.style == "chevron") {
            introns <- style_intron_chevron(coords, linejoin, chevron.height)
            introns$name <- grid::grobName(introns, "introns")
            genemodel <- grid::gTree(children = grid::gList(introns, exons))
            genemodel$name <- grid::grobName(genemodel, "genemodel")
            return(genemodel)
        }

        if (intron.style == "arrowline") {
            introns <- style_intron_arrowline(coords, linejoin, arrow, arrow.freq)
            introns$name <- grid::grobName(introns, "introns")
            genemodel <- grid::gTree(children = grid::gList(introns, exons),
                                     cl = "genemodel")
            genemodel$name <- grid::grobName(genemodel, "genemodel")
            return(genemodel)
        }
    }
)


# Intron Styles -----------------------------------------------------------

style_intron_arrowline <- function(
    data, linejoin = "mitre",
    arrow.template = NULL, arrow.freq = unit(4, "mm")
) {
    data <- lapply(split(data, data$group), function(group) {
        extremes <- group[which.max(group$xmax), ]
        extremes$xmin <- min(group$xmin)
        extremes$y <- (extremes$ymax + extremes$ymin) / 2
        extremes
    })
    data <- do.call(rbind, data)
    data$xmin <- unit(data$xmin, "npc")
    data$xmax <- unit(data$xmax, "npc")

    if (is.null(arrow.template)) {
        arrow.template <- arrow(length = unit(2, "mm"))
    }

    grid::grob(
        data = data,
        arrow.template = arrow.template,
        arrow_freq_number = as.numeric(arrow.freq),
        arrow_freq_unit = attr(arrow.freq, "unit"),
        gp = grid::gpar(
            col  = data$colour,
            fill = data$fill,
            lwd  = data$size * .pt,
            lty  = data$linetype,
            linejoin = linejoin,
            lineend = if (identical(linejoin, "round")) "round" else "butt"
        ),
        cl = "arrowline"
    )
}

style_intron_plainline <- function(data, linejoin) {
    data <- lapply(split(data, data$group), function(group) {
        extremes <- group[which.max(group$xmax), ]
        extremes$xmin <- min(group$xmin)
        extremes$y <- (extremes$ymax + extremes$ymin) / 2
        extremes
    })
    data <- do.call(rbind, data)
    data$xmin <- unit(data$xmin, "npc")
    data$xmax <- unit(data$xmax, "npc")

    grid::segmentsGrob(
        x0 = data$xmin,
        x1 = data$xmax,
        y0 = data$y,
        y1 = data$y,
        gp = grid::gpar(
            col  = data$colour,
            lwd  = data$size * .pt,
            lty  = data$linetype,
            linejoin = linejoin,
            lineend = if (identical(linejoin, "round")) "round" else "butt"
        )
    )
}

style_intron_chevron <- function(data, linejoin = "bevel", height = 1) {
    dropcols <- !(colnames(data) %in% c("xmin", "xmax", "ymin", "ymax", "fill"))
    data <- split(data, data$group)
    data <- lapply(seq_along(data), function(i) {
        group <- data[[i]]
        n <- nrow(group)
        if (n == 1) {
            return(NULL)
        }
        ymid <- (group$ymin[1] + group$ymax[1]) / 2
        ymax <- if (group$strand[1] == "+") max(group$ymax) else min(group$ymin)
        ymax <- ymid + diff(c(ymid, ymax)) * height
        group <- group[order(group$xmin, group$xmax),]
        out <- data.frame(
            x = c(head(group$xmax, -1), tail(group$xmin, -1),
                  (head(group$xmax, -1) + tail(group$xmin, -1)) / 2),
            y = rep(c(ymid, ymax),
                    c(2 * n - 2, n - 1)),
            id = i,
            row.names = NULL
        )
        out <- out[order(out$x), ]
        data.frame(out, group[1, dropcols], row.names = NULL)
    })
    data <- do.call(rbind, data)
    grid::polylineGrob(
        x = data$x,
        y = data$y,
        id = data$id,
        gp = grid::gpar(
            col  = data$colour[!duplicated(data$id)],
            lwd  = data$size * .pt,
            lty  = data$linetype,
            linejoin = "bevel",
            lineend = if (identical(linejoin, "round")) "round" else "butt"
        )
    )
}

# makeContent -------------------------------------------------------------

# Needs to be exported to get the arrow thing to work

#' @export
#' @rdname geom_genemodel
#' @usage NULL
makeContent.genemodel <- function(x) {
    # Evaluate arrowline grob to fix colour mistakes
    if (inherits(x$children[[1]], "arrowline")) {
        x$children[[1]] <- grid::makeContent(x$children[[1]])
    }
    x
}

makeContent.arrowline <- function(x) {

    dat <- x$data
    x$data <- NULL

    # Convert coordinates to absolute
    dat$xmin <- grid::convertX(dat$xmin, x$arrow_freq_unit, TRUE)
    dat$xmax <- grid::convertX(dat$xmax, x$arrow_freq_unit, TRUE)

    dat <- split(dat, dat$group)
    dat <- lapply(dat, function(s){
        # Split up lines into parts
        munched <- if (s$strand == "+") {
            c(seq(min(s$xmin), max(s$xmax), by = x$arrow_freq_number),
              max(s$xmax))
        } else {
            c(seq(max(s$xmax), min(s$xmin), by = -x$arrow_freq_number),
              min(s$xmin))
        }
        offset <- if (length(munched) > 1) -1 else length(munched)
        trans <- data.frame(
            xmin = head(munched, offset),
            xmax = tail(munched, offset),
            s[1, !(colnames(s) %in% c("xmin", "xmax"))],
            arrow_length = c(rep(1, length(munched) - 2), 0),
            row.names = NULL,
            stringsAsFactors = FALSE
        )
    })
    dat <- do.call(rbind, dat)

    arrow <- x$arrow.template
    arrow$length <- unit(dat$arrow_length * as.numeric(arrow$length),
                         attr(arrow$length, "unit"))

    # Update grob
    x$gp <- grid::gpar(
        col  = dat$colour,
        fill = dat$fill,
        lwd = dat$size * .pt,
        lty = dat$linetype,
        linejoin = x$linejoin,
        lineend = if (identical(x$linejoin, "round")) "round" else "butt"
    )

    x$x0 <- unit(dat$xmin, x$arrow_freq_unit)
    x$x1 <- unit(dat$xmax, x$arrow_freq_unit)
    x$y0 <- unit(dat$y, "npc")
    x$y1 <- unit(dat$y, "npc")
    x$arrow <- arrow

    # Re-class grob
    x$cl <- "segments"
    class(x)[1] <- "segments"
    x
}

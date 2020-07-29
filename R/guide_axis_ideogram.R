# Constructor -------------------------------------------------------------

#' Ideogram axis guide
#'
#' Places an ideogram next to the axis and indicates the data range interpreted
#' as genomic positions on the ideogram. The ideograms need to be setup first
#' with the \code{setup_ideogram()} function.
#'
#' @inheritParams ggplot2::guide_axis
#' @param chromosome A \code{character} with chromosome name of length one, for
#'   example \code{"chr12"}.
#' @param genome A \code{character} with a genome name of length one, for
#'   example \code{"hg38"}.
#' @param ideo_size A \code{unit} of length one indicating the size of the
#'   ideogram. If a \code{numeric} is provided, this is interpreted as
#'   millimetre units. A small proportional margin will be added for an
#'   indicator.
#' @param funnel_size A \code{unit} of length one indicating the size of the
#'   funnel space. If a \code{numeric} is provided, this is interpreted as
#'   millimetre units. If this can be interpreted as zero, no funnel will be
#'   drawn.
#' @param funnel_col A \code{character} of length one setting the colour for the
#'   funnel and indicator. Set to \code{NA} to make the funnel and indicator
#'   transparent.
#' @param regular_axis A \code{logical} of length one: should the ticks and
#'   labels of the usual position axis be drawn? (default: \code{TRUE}).
#'
#' @details Currently does not support S4Vector or genomic scales (yet).
#'
#' @return A list with the S3 class \code{guide}.
#' @family position guides
#' @seealso The \code{\link[ggnomics]{setup_ideograms}()} function for importing
#'   and caching ideogram data.
#' @export
#'
#' @examples
#' # Setup ideograms and make a basic plot
#' setup_ideograms(genome = "hg38")
#' p <- ggplot(iris, aes(Sepal.Width * 1e7, Sepal.Length * 1e7)) +
#'     geom_point()
#'
#' # As main axis
#' p + guides(x = guide_ideogram_axis("hg38", chromosome = "chr2"))
#'
#' # As secundary axis
#' p + guides(x.sec = guide_ideogram_axis("hg38", chromosome = "chr2",
#'                                        regular_axis = FALSE))
#'
#' # Without funnel
#' p + guides(x = guide_ideogram_axis("hg38", chromosome = "chr2",
#'                                    funnel_size = 0))
#'
#' # Without funnel or indicator
#' p + guides(x = guide_ideogram_axis("hg38", chromosome = "chr2",
#'                                    funnel_size = 0, funnel_col = NA))
guide_ideogram_axis <- function(title = waiver(), check.overlap = FALSE,
                                angle = NULL, n.dodge = 1,
                                order = 0, position = waiver(),
                                chromosome = NULL,
                                genome = NULL,
                                ideo_size = unit(0.5, "cm"),
                                funnel_size = unit(0.5, "cm"),
                                funnel_col = "dodgerblue",
                                regular_axis = TRUE
                                ) {
    try_require("polyclip", "guide_ideogram_axis")

    if (!grid::is.unit(ideo_size)) {
        size <- unit(size, "mm")
    }
    if (!grid::is.unit(funnel_size)) {
        funnel_size <- unit(funnel_size, "mm")
    }

    ideograms <- ideo_cache_get(genome)
    structure(
        list(
            title = title,
            check.overlap = check.overlap,
            angle = angle,
            n.dodge = n.dodge,
            order = order,
            position = position,
            available_aes = c("x", "y"),
            chromosome = chromosome,
            ideograms = ideograms,
            name = "ideogram_axis",
            ideo_size = ideo_size,
            funnel_size = funnel_size,
            funnel_col = funnel_col,
            regular_axis = regular_axis
        ),
        class = c("guide", "ideogram_axis", "axis")
    )
}

# Methods -----------------------------------------------------------------

#' @export
#' @describeIn guide_ideogram_axis Trainer for ideogram axis. See
#'   \code{\link[ggplot2]{guide-exts}}.
#' @usage NULL
guide_train.ideogram_axis <- function(guide, scale, aesthetic = NULL) {
    guide <- NextMethod()

    limits <- scale$get_limits()
    if (!inherits(limits, "GenomicRanges")) {
        chrom <- guide$chromosome
        if (is.null(chrom)) {
            stop("Please provide the `chromosome` argument to ",
                 "`guide_axis_ideogram()`.", call. = FALSE)
        }
    } else {
        chrom <- unique(seqnames(limits))
    }

    if (!all(chrom %in% names(guide$ideograms))) {
        stop("Not all chromosomes found in ideograms cache.",
             call. = FALSE)
    }

    guide$ideograms <- guide$ideograms[chrom]
    guide$limits <- limits
    guide
}

#' @export
#' @describeIn guide_ideogram_axis Transformer for ideogram axis. See
#'   \code{\link[ggplot2]{guide-exts}}.
#' @usage NULL
guide_transform.ideogram_axis <- function(guide, coord, panel_params) {
    guide <- NextMethod()
    aesthetic <- names(guide$key)
    aesthetic <- aesthetic[!grepl("^\\.", aesthetic)][1]
    lims <- guide$limits
    lims <- data.frame(a = lims)
    colnames(lims) <- aesthetic
    lims <- coord$transform(lims, panel_params)
    guide$scaled_limits <- unlist(lims)
    guide
}

#' @export
#' @describeIn guide_ideogram_axis Graphic object generator for ideogram axis.
#'   See \code{\link[ggplot2]{guide-exts}}.
#' @usage NULL
guide_gengrob.ideogram_axis <- function(guide, theme) {
    aesthetic <- names(guide$key)
    aesthetic <- aesthetic[!grepl("^\\.", aesthetic)][1]

    axis_position <- match.arg(substr(guide$position, 1, 1),
                               c("t", "b", "r", "l"))

    ideo <- build_ideogram(
        position = axis_position,
        ideograms = guide$ideograms,
        limits = guide$limits,
        scaled_limits = guide$scaled_limits,
        size = guide$ideo_size,
        funnel_col = guide$funnel_col
    )

    draw_ideogram_axis(
        break_positions = guide$key[[aesthetic]],
        break_labels = guide$key$.label,
        axis_position = guide$position,
        theme = theme,
        check.overlap = guide$check.overlap,
        angle = guide$angle,
        n.dodge = guide$n.dodge,
        ideograms = ideo,
        ideo_size = guide$ideo_size,
        funnel_size = guide$funnel_size,
        regular_axis = guide$regular_axis
    )
}

# Drawing orchestrator ----------------------------------------------------

draw_ideogram_axis <- function(
    break_positions,
    break_labels,
    axis_position,
    theme,
    check.overlap = FALSE,
    angle = NULL,
    n.dodge = 1,
    ideograms = list(),
    ideo_size = unit(0.5, "cm"),
    funnel_size = unit(0.5, "cm"),
    regular_axis = TRUE
) {
    axis_position <- match.arg(substr(axis_position, 1, 1),
                               c("t", "b", "r", "l"))
    aes <- if (axis_position %in% c("t", "b")) "x" else "y"

    elements <- build_axis_elements(axis_position, angle, theme)

    params <- setup_axis_params(axis_position)
    line_grob <- build_axis_line(elements$line, params)


    n_breaks <- length(break_positions)

    label_grobs <- if (n_breaks & regular_axis) {
        build_axis_labels(
            elements,
            labels = break_labels,
            position = break_positions,
            dodge = n.dodge, check.overlap = check.overlap, params = params
        )
    } else {
        list(zeroGrob())
    }


    # Setup ticks

    if (n_breaks & regular_axis) {
        sizes <- unit.c(elements$tick_length)
        tick_grob <- build_axis_ticks(elements$ticks, sizes,
                                      break_positions, params)
        elements$tick_length <- max(sizes)
    } else {
        tick_grob <- zeroGrob()
        elements$tick_length <- unit(0, "mm")
    }


    assemble_axis_grobs_ideogram(
        ticks = tick_grob, labels = label_grobs,
        lines = line_grob, elements = elements,
        params = params, ideo = ideograms, size = ideo_size,
        funnel_size = funnel_size
    )
}

# Drawing details ---------------------------------------------------------

build_axis_elements <- function(axis_position = "b", angle = NULL, theme) {
    aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"
    axis_position <- match.arg(axis_position, c("top", "bottom", "left", "right"))

    element_names = c(line = "axis.line.", ticks = "axis.ticks.",
                      tick_length = "axis.ticks.length.", label = "axis.text.")
    element_names <- setNames(paste0(element_names, aesthetic, ".",
                                     axis_position),
                              names(element_names))
    elements <- lapply(element_names, calc_element, theme)

    if (inherits(elements$label, "element_text")) {
        lab_overrides <- .int$axis_label_element_overrides(axis_position, angle)
        elements$label$angle <- lab_overrides$angle %||% elements$label$angle
        elements$label$hjust <- lab_overrides$hjust %||% elements$label$hjust
        elements$label$vjust <- lab_overrides$vjust %||% elements$label$vjust
    }
    elements
}

build_axis_line <- function(element, params) {
    args <- list(element, unit(c(0, 1), "npc"), rep(params$pos, 2L))
    names(args) <- c("element", params$aes, params$non_aes)
    do.call(element_grob, args)
}

build_axis_labels <- function(
    elements, labels, position, dodge = 1, check.overlap = FALSE, params
) {
    n_breaks <- length(position)
    if (n_breaks == 0) {
        return(list(zeroGrob()))
    }

    # Validate labels
    if (is.list(labels)) {
        if (any(vapply(labels, is.language, logical(1)))) {
            labels <- do.call(expression, labels)
        } else {
            labels <- unlist(labels)
        }
    }

    dodge_pos <- rep(seq_len(dodge), length.out = n_breaks)
    dodge_idxs <- split(seq_len(n_breaks), dodge_pos)
    label_grobs <- lapply(dodge_idxs, function(idx) {
        .int$draw_axis_labels(
            break_positions = position[idx],
            break_labels = labels[idx],
            label_element = elements$label,
            is_vertical = params$vertical,
            check.overlap = check.overlap
        )
    })
}

build_axis_ticks <- function(element, length, position, params) {
    n_breaks <- length(position)
    pos <- unit(c(params$pos, params$pos + (params$tick_dir * 1)), "npc")
    pos <- rep(pos[params$tick_ord], times = n_breaks)

    args <- list(element, unit(rep(position, each = 2), "native"),
                 pos, rep(2, times = n_breaks))
    names(args) <- c("element", params$aes, params$non_aes, "id.lengths")

    do.call(element_grob, args)
}

assemble_axis_grobs_ideogram <-
    function(ticks, labels, lines, elements, params, ideo, size = unit(0.5, "cm"),
             funnel_size = unit(0.5, "cm")) {
        non_dims <- paste0(params$non_dim, "s")
        label_dims <- do.call(unit.c, lapply(labels, params$labels_measure))
        grobs <- c(list(ticks), labels, list(ideo$ideo))
        validgrob <- !vapply(grobs, inherits, logical(1), "zeroGrob")
        grob_dims <- unit.c(elements$tick_length, label_dims, size * (1 + sqrt(5)) * 0.5)

        if (params$labels_first) {
            grobs <- rev(grobs)
            grob_dims <- rev(grob_dims)
        }

        gt <- base::do.call(
            params$gtable_element,
            setNames(list("axis", grobs, grob_dims, unit(1, "npc")),
                     c("name", "grobs", non_dims, params$dim))
        )

        # Add funnel
        add_funnel <- convertUnit(funnel_size, "mm", valueOnly = TRUE) > 0
        if (dim(gt)[2] > 1 && add_funnel) {
            gt <- gtable::gtable_add_cols(
                gt, funnel_size, if (params$labels_first) 1 else 2
            )
            gt <- gtable::gtable_add_grob(
                gt, ideo$funnel, t = 1, z = -1,
                l = if (params$labels_first) 2 else 1,
                r = if (params$labels_first) 4 else 3
            )
        } else if (add_funnel) {
            gt <- gtable::gtable_add_rows(
                gt, funnel_size, if (params$labels_first) 1 else 2
            )
            gt <- gtable::gtable_add_grob(
                gt, ideo$funnel, l = 1, z = -1,
                t = if (params$labels_first) 2 else 1,
                b = if (params$labels_first) 4 else 3,
            )
        }


        # create viewport
        justvp <- base::do.call(
            grid::viewport,
            setNames(list(params$pos, params$gtable_measure(gt),
                          params$opposite_axis),
                     c(params$non_aes, params$non_dim, "just"))
        )

        gTree(children = gList(lines, gt),
              width = gtable::gtable_width(gt),
              height = gtable::gtable_height(gt),
              xmin = NULL, ymin = NULL, vp = justvp,
              cl = "absoluteGrob")
    }



setup_axis_params <- function(axis_position) {
    aesthetic <- if (axis_position %in% c("t", "b")) "x" else "y"

    # Set verticality parameters
    if (is_vertical <- axis_position %in% c("l", "r")) {
        position_dim <- "y"
        position_size <- "height"
        gtable_element <- gtable::gtable_row
        gtable_measure <- gtable::gtable_width
        measure_labels <- grid::grobWidth
    } else {
        position_dim <- "x"
        position_size <- "width"
        gtable_element <- gtable::gtable_col
        gtable_measure <- gtable::gtable_height
        measure_labels <- grid::grobHeight
    }
    non_position_dim <- setdiff(c("x", "y"), position_dim)
    non_position_size <- setdiff(c("width", "height"), position_size)

    # Set secondarity parameters
    if (is_second <- axis_position %in% c("r", "t")) {
        tick_direction <- 1
        non_position_panel <- 0
        tick_coord_order <- c(2, 1)
    } else {
        tick_direction <- -1
        non_position_panel <- 1
        tick_coord_order <- c(1, 2)
    }

    labels_first <- axis_position %in% c("l", "t")
    axis_opposite <- chartr("tblr", "btrl", axis_position)
    axis_opposite <- match.arg(axis_opposite, c("top", "bottom", "left", "right"))

    list(aes = aesthetic, non_aes = non_position_dim,
         dim = position_size, non_dim = non_position_size,
         gtable_element = gtable_element,
         gtable_measure = gtable_measure,
         labels_measure = measure_labels,
         tick_dir = tick_direction,
         tick_ord = tick_coord_order,
         pos = non_position_panel,
         labels_first = labels_first,
         opposite_axis = axis_opposite,
         vertical = is_vertical)
}

build_ideogram <- function(position = "b", ideograms,
                           limits, scaled_limits, size = unit(5, "mm"),
                           funnel_col = NA
                    ) {
    horizontal <- position %in% c("b", "t")
    offset <- ((size * (1 + sqrt(5)) * 0.5) - size) * 0.5

    lims <- range(c(ideograms[[1]]$bands$x, ideograms[[1]]$outline$x, limits))

    bands <- ideograms[[1]]$bands
    bands$x <- scales::rescale(bands$x, from = lims)
    gp <- gpar(fill = bands$fill[!duplicated(bands$id)], col = NA)

    bands <- if (horizontal) {
        polygonGrob(x = bands$x, y = bands$y * size + offset, id = bands$id, gp = gp)
    } else {
        polygonGrob(x = bands$y * size + offset, y = bands$x, id = bands$id, gp = gp)
    }

    outline <- ideograms[[1]]$outline
    outline$x <- scales::rescale(outline$x, from = lims)

    outline <- if (horizontal) {
        polylineGrob(x = outline$x, y = outline$y * size + offset)
    } else {
        polylineGrob(x = outline$y * size + offset, y = outline$x)
    }

    limits <- scales::rescale(limits, from = lims)
    gp_hilight <- gpar(fill = funnel_col, col = NA)
    hilight <- if (horizontal) {
        rectGrob(x = limits[1], width = diff(limits), hjust = 0,
                 gp = gp_hilight)
    } else {
        rectGrob(y = limits[1], height = diff(limits), vjust = 0,
                 gp = gp_hilight)
    }

    ideogram <- grobTree(hilight = hilight, bands = bands, outline = outline,
                         cl = "ideogrob")

    funnel <- if (position == "b") {
        polygonGrob(
            x = c(rev(scaled_limits), limits), y = c(1, 1, 0, 0),
            gp = gp_hilight
        )
    } else if (position == "t") {
        polygonGrob(
            x = c(rev(scaled_limits), limits), y = c(0, 0, 1, 1),
            gp = gp_hilight
        )
    } else if (position == "l") {
        polygonGrob(
            x = c(0, 0, 1, 1), y = c(limits, rev(scaled_limits)),
            gp = gp_hilight
        )
    } else {
        polygonGrob(
            x = c(1, 1, 0, 0), y = c(limits, rev(scaled_limits)),
            gp = gp_hilight
        )
    }

    list(ideo = ideogram, funnel = funnel)
}

# Drawtime edits ----------------------------------------------------------

#' @export
#' @noRd
#' @keywords internal
makeContext.ideogrob <- function(x) {

    grob <- x
    is_polygon <- which(vapply(x$children, inherits, logical(1), "polygon"))
    is_polyline <- which(vapply(x$children, inherits, logical(1), "polyline"))

    gon <- x$children[[is_polygon]]
    line <- x$children[[is_polyline]]

    outline <- list(
        x = convertX(line$x, "mm", valueOnly = TRUE),
        y = convertY(line$y, "mm", valueOnly = TRUE)
    )
    ranges <- list(x = range(outline$x), y = range(outline$y))
    mids <- lapply(ranges, diff)
    r <- min(unlist(mids))/2

    centro <- length(outline$x) == 10
    x <- outline$x
    y <- outline$y

    # Find corner triplets that need to be rounded
    i <- if (centro) {
        list(c(10, 1, 2), c(1, 2, 3), c(3, 4, 5),  c(4, 5, 6),
             c(5, 6, 7),  c(6, 7, 8), c(8, 9, 10), c(9, 10, 1))
    } else {
        list(c(1, 2, 3), c(2, 3, 4), c(3, 4, 1), c(4, 1, 2))
    }

    corners <- lapply(i, function(ii) {
        roundcorner(x[ii], y[ii], r)
    })
    cx <- lapply(corners, `[[`, "x")
    cy <- lapply(corners, `[[`, "y")

    outline2 <- if (centro) {
        # Reorder correctly
        list(
            x = c(cx[[1]], cx[[2]], x[3], cx[[3]], cx[[4]],
                  cx[[5]], cx[[6]], x[8], cx[[7]], cx[[8]]),
            y = c(cy[[1]], cy[[2]], y[3], cy[[3]], cy[[4]],
                  cy[[5]], cy[[6]], y[8], cy[[7]], cy[[8]])
        )
    } else {
        list(x = unlist(cx), y = unlist(cy))
    }

    bands <- data.frame(
        x = convertX(gon$x, "mm", valueOnly = TRUE),
        y = convertY(gon$y, "mm", valueOnly = TRUE),
        id = gon$id
    )
    bands <- split(bands[, 1:2], bands$id)

    # Clip bands by outline
    bands <- unlist(lapply(bands, polyclip::polyclip,
                           B = outline2, closed = TRUE), recursive = FALSE)
    x <- lapply(bands, `[[`, "x")
    y <- lapply(bands, `[[`, "y")

    grob$children[[is_polygon]] <- polygonGrob(
        x = unit(unlist(x), "mm"),
        y = unit(unlist(y), "mm"),
        id = rep(seq_along(x), lengths(x)),
        gp = gon$gp
    )

    grob$children[[is_polyline]] <- polylineGrob(
        x = c(outline2$x, outline2$x[1]),
        y = c(outline2$y, outline2$y[1]),
        default.units = "mm"
    )

    return(grob)
}

#' Rounding off corners
#'
#' @param x,y A \code{numeric} vector of length three describing the x- and
#'   y-coordinates of three points, wherein the second point is a corner.
#' @param r A \code{numeric} radius.
#' @param n An \code{integer} specifying how many points on an arc should be
#'   drawn.
#'
#' @return A \code{list} with the elements \code{x} and \code{y}
#'
#' @note The code is loosely translated from C# to R based on the following
#' stackoverflow post: https://stackoverflow.com/questions/24771828/algorithm-for-creating-rounded-corners-in-a-polygon
#' @noRd
#' @keywords internal
#' @examples
#' roundcorner(x = c(0, 1, 1), y = c(0, 0, 1), r = 0.5)
roundcorner <- function(x, y, r, n = 10) {
    # Calculate angle of corner
    angle <- atan2(y[2] - y[1], x[2] - x[1]) - atan2(y[2] - y[3], x[2] - x[3])
    # Length to intersection points
    seg <- r / abs(tan(angle / 2))

    # Distance from first/last point to corner
    len1 <- sqrt((x[2] - x[1])^2 + (y[2] - y[1])^2)
    len2 <- sqrt((x[2] - x[3])^2 + (y[2] - y[3])^2)
    len <- min(len1, len2)
    if (seg > len) {
        seg <- len
        r <- seg * abs(tan(angle / 2))
    }
    # Circle center to corner distance
    c2corner <- sqrt(r^2 + seg ^2)
    # Calculate intersection point coordinates
    i1x <- x[2] - (x[2] - x[1]) * seg / len1
    i1y <- y[2] - (y[2] - y[1]) * seg / len1
    i2x <- x[2] - (x[2] - x[3]) * seg / len2
    i2y <- y[2] - (y[2] - y[3]) * seg / len2
    # Add vectors
    cx <- i1x + i2x - x[2]
    cy <- i1y + i2y - y[2]
    # Difference between corner to added vectors
    dx <- x[2] - cx
    dy <- y[2] - cy
    dist <- sqrt(dx^2 + dy^2)

    # Get circle coordinates
    circx <- x[2] - dx * c2corner / dist
    circy <- y[2] - dy * c2corner / dist

    # Calculate start/end angles
    start <- atan2((i1y - circy), (i1x - circx))
    end   <- atan2((i2y - circy), (i2x - circx))
    sweep <- end - start

    # Correct for phase difference
    sweep <- sweep - (2 * pi) * sign(sweep) * (abs(sweep) > pi)

    sweep <- seq(start, start + sweep, length.out = n)

    list(
        x = cos(sweep) * r + circx,
        y = sin(sweep) * r + circy
    )
}

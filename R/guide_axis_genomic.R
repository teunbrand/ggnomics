# Constructor -------------------------------------------------------------

#' @name guide_genomic_axis
#' @title Axis for genomic positions
#'
#' @description This axis guide is the genomic equivalent of
#' \code{\link[ggplot2]{guide_axis}} to pair with the genomic position scales
#' \code{\link[=scale_genomic]{scale_(x|y)_genomic()}}.
#'
#' @inheritParams ggplot2::guide_axis
#'
#' @details This guide places the sequence names in the middle or their limits
#'   and places label for minor breaks at positions along the sequences.
#'   Defaults to \code{guide_axis} behaviour if not exposed to genomic data.
#'
#' @return A \code{guide} object.
#'
#' @export
#'
#' @examples
#' NULL
guide_genomic_axis <- function(
  title = waiver(),
  check.overlap = FALSE,
  angle = NULL,
  n.dodge = 1,
  order = 0,
  position = waiver()
) {
  structure(
    list(
      title = title,
      check.overlap = check.overlap,
      angle = angle,
      n.dodge = n.dodge,
      order = order,
      position = position,
      available_aes = c("x", "y"),
      name = "genomic_axis"
    ),
    class = c("guide", "genomic_axis", "axis")
  )
}

# Trainer -----------------------------------------------------------------

#' @export
#' @describeIn guide_genomic_axis Trainer for genomic axis. See
#'   \code{\link[ggplot2]{guide-exts}}.
#' @usage NULL
guide_train.genomic_axis <- function(guide, scale, aesthetic = NULL) {
    aesthetic <- aesthetic %||% scale$aesthetics[1]
    majorbreaks <- scale$get_breaks()

    # Doesn't make sense to use this axis if the data isn't genomic or scale
    # doesn't have a labeller for minor breaks.
    if (!inherits(Nightfall(majorbreaks), 'ANYGenomic') ||
        !("get_labels_minor" %in% union(names(scale), names(scale$super())))) {
        guide <- NextMethod()
        class(guide) <- setdiff(class(guide), "genomic_axis")
        return(guide)
    }

    minorbreaks <- scale$get_breaks_minor()
    lens <- c(length(majorbreaks), length(minorbreaks))

    # Make a data.frame for empty ticks
    empty_ticks <- .int$new_data_frame(
        list(aesthetic = numeric(), .value = numeric(),
             .label = character())
    )
    names(empty_ticks) <- c(aesthetic, ".value", ".label")

    if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
        warning("genomic_axis guide needs appropriate scales: ",
                guide$available_aes)
        guide$key <- empty_ticks
        guide$key_minor <- empty_ticks
    } else {

        guide$key <- format_guide_key(
            breaks = majorbreaks, scale = scale, prototype = empty_ticks,
            aesthetic = aesthetic, type = "major"
        )
        guide$key_minor <- format_guide_key(
            breaks = minorbreaks, scale = scale, prototype = empty_ticks,
            aesthetic = aesthetic, type = "minor"
        )
    }

    guide$name <- paste0(guide$name, "_", aesthetic)
    guide$hash <- digest::digest(list(guide$title, guide$key$.value,
                                      guide$key$.label, guide$name))
    guide
}

# Transformer -------------------------------------------------------------

#' @export
#' @describeIn guide_genomic_axis Transformer for genomic axis. See
#'   \code{\link[ggplot2]{guide-exts}}.
#' @usage NULL
guide_transform.genomic_axis <- function(guide, coord, panel_params) {
  if (is.null(guide$position) || nrow(guide$key) == 0) {
    return(guide)
  }
  aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))]

  if (all(c("x", "y") %in% aesthetics)) {
    guide$key <- coord$transform(guide$key, panel_params)
    guide$key_minor <- coord$transform(guide$key_minor, panel_params)
  } else {
    other_aesthetic <- setdiff(c("x", "y"), aesthetics)
    override_value <- if (guide$position %in% c("bottom", "left")) -Inf else Inf
    guide$key[[other_aesthetic]] <- override_value
    guide$key_minor[[other_aesthetic]] <- override_value
    guide$key <- coord$transform(guide$key, panel_params)
    guide$key_minor <- coord$transform(guide$key_minor, panel_params)
    .int$warn_for_guide_position(guide)
  }

  # Average positions of major labels
  major <- guide$key
  aa <- split(major[aesthetics], factor(major$.label,
                                        levels = unique(major$.label)))
  aa <- matrix(vapply(aa, colMeans, numeric(length(aesthetics)),
                      USE.NAMES = FALSE),
               ncol = length(aesthetics))
  aa <- lapply(seq_along(aesthetics), function(i){aa[,i]})
  major <- major[!duplicated(major$.label), ]
  major[aesthetics] <- aa

  guide$key <- major
  guide
}

# Grob generator ----------------------------------------------------------

#' @export
#' @describeIn guide_genomic_axis Graphic object generator for genomic axis. See
#'   \code{\link[ggplot2]{guide-exts}}.
#' @usage NULL
guide_gengrob.genomic_axis <- function(guide, theme) {
    aesthetics <- names(guide$key)[!grepl("^\\.", names(guide$key))][1]

    draw_genomic_axis(
        break_positions = guide$key[[aesthetics]],
        break_pos_minor = guide$key_minor[[aesthetics]],
        break_labels = guide$key$.label,
        break_lab_minor = guide$key_minor$.label,
        axis_position = guide$position,
        theme = theme,
        check.overlap = guide$check.overlap,
        angle = guide$angle,
        n.dodge = guide$n.dodge
    )
}

# Drawing function --------------------------------------------------------

draw_genomic_axis <- function(
    break_positions,
    break_pos_minor,
    break_labels,
    break_lab_minor,
    axis_position,
    theme,
    check.overlap = FALSE,
    angle = NULL,
    n.dodge = 1
) {
  # Setup assumptions
  axis_position <- match.arg(axis_position, c("top", "bottom", "right", "left"))
  aesthetic <- if (axis_position %in% c("top", "bottom")) "x" else "y"
  labels_first_gtable <- axis_position %in% c("left", "top")

  # Do vertical vs horizontal
  is_vertical <- axis_position %in% c("left", "right")
  if (is_vertical) {
    position_size <- "height"
    non_position_size <- "width"
    gtable_element <- gtable::gtable_row
    measure_gtable <- gtable::gtable_width
    measure_labels_non_pos <- grid::grobWidth
  } else {
    position_size <- "width"
    non_position_size <- "height"
    gtable_element <- gtable::gtable_col
    measure_gtable <- gtable::gtable_width
    measure_labels_non_pos <- grid::grobHeight
  }

  # Do primary vs secondary
  if (axis_position %in% c("right", "top")) {
    non_position_panel <- unit(0, "npc")
  } else {
    non_position_panel <- unit(1, "npc")
  }

  # Build axis line
  line_grob <- setup_axis_line(axis_position, theme)

  # Setup breaks
  n_breaks <- length(break_pos_minor)
  opposite_positions <- setNames(c("bottom", "top", "left", "right"),
                                 c("top", "bottom", "right", "left"))
  axis_position_opposite <- unname(opposite_positions[axis_position])

  # Return empty
  if (n_breaks == 0) {
    return(grid::gTree(
      children = grid::gList(line_grob),
      width = grid::grobWidth(line_grob),
      height = grid::grobHeight(line_grob),
      cl = "abosluteGrob"
    ))
  }

  # Setup labels
  label_grobs <- setup_axis_labels(
      major = break_labels, minor = break_lab_minor,
      major_pos = break_positions, minor_pos = break_pos_minor,
      position = axis_position, theme = theme,
      check.overlap = check.overlap,
      angle = angle, n.dodge = 1
  )

  # Setup tickmarks
  ticks <- setup_tickmarks(break_pos_minor, axis_position, theme)

  # Combine ticks and labels
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- base::do.call(grid::unit.c, lapply(label_grobs,
                                                   measure_labels_non_pos))
  grobs <- c(list(ticks$grob), label_grobs)
  grob_dims <- grid::unit.c(ticks$size, label_dims)

  if (labels_first_gtable) {
    grobs <- rev(grobs)
    grob_dims <- rev(grob_dims)
  }

  # Build final grob
  gt <- base::do.call(
    gtable_element,
    setNames(list("axis", grobs, grob_dims, unit(1, "npc")),
             c("name", "grobs", non_position_sizes, position_size))
  )

  # Build viewport for text justification
  justvp <- base::do.call(
    grid::viewport,
    setNames(list(non_position_panel, measure_gtable(gt),
                  axis_position_opposite),
             c(setdiff(c("x", "y"), aesthetic), non_position_size, "just"))
  )

  # Comine the lot
  grid::gTree(
    children = grid::gList(line_grob, gt),
    width = gtable::gtable_width(gt),
    height = gtable::gtable_height(gt),
    vp = justvp,
    cl = "absoluteGrob"
  )
}

# Helper functions --------------------------------------------------------

format_guide_key <- function(breaks = NULL,
                             scale,
                             prototype,
                             aesthetic = "x",
                             type = "major")
{
    if (length(breaks) == 0) {
        return(prototype)
    } else {
        if (scale$is_discrete()) {
            mapped <- scale$map(breaks)
        } else {
            mapped <- breaks
        }
        key <- .int$new_data_frame(setNames(list(mapped), aesthetic))
        key$.value <- breaks
        if (type == "minor" && !is.null(scale$get_labels_minor)) {
            key$.label <- scale$get_labels_minor(breaks)
        } else {
            key$.label <- scale$get_labels(breaks)
        }
        key$.label <- validate_labels(key$.label)
        key <- key[is.finite(key[[aesthetic]]), ]
        return(key)
    }
}

validate_labels <- function(labels) {
    if (is.list(labels)) {
        if (any(vapply(labels, is.language, logical(1)))) {
            labels <- base::do.call(expression, labels)
        } else {
            labels <- unlist(labels)
        }
    }
    labels
}

# Helper for axis line in draw function
setup_axis_line <- function(
  position, theme
) {
  aesthetic <- if (position %in% c("top", "bottom")) "x" else "y"

  alt <- if (position %in% c("right", "top")) unit(0, "npc") else unit(1, "npc")

  # Resolve elements
  line_element_name <- paste0("axis.line.", aesthetic, ".", position)
  element <- calc_element(line_element_name, theme)

  line_grob <- base::do.call(
    element_grob,
    setNames(
      list(
        element,
        unit(c(0, 1), "npc"),
        grid::unit.c(alt, alt)
      ),
      c("element", aesthetic, setdiff(c("x", "y"), aesthetic))
    )
  )
}

# Helper for tickmarks in draw function
setup_tickmarks <- function(
    break_pos,
    position,
    theme
) {
    aesthetic <- if (position %in% c("top", "bottom")) "x" else "y"

    # Calculate elements
    element_name <- paste0("axis.ticks.", aesthetic, ".", position)
    element_size <- paste0(
        "axis.ticks.length.", aesthetic, ".", position
    )
    element <- calc_element(element_name, theme)
    size    <- calc_element(element_size, theme)

    # Switch primary/secondary
    if (position %in% c("right", "top")) {
        dir <- 1
        alt <- unit(0, "npc")
        ord <- c(2, 1)
    } else {
        dir <- -1
        alt <- unit(1, "npc")
        ord <- c(1, 2)
    }

    n_breaks <- length(break_pos)
    args <- list(
        element,
        rep(unit(break_pos, "native"), each = 2),
        rep(grid::unit.c(alt + (dir * size),
                         alt)[ord],
            times = n_breaks),
        rep(2, times = n_breaks)
    )
    args <- setNames(args, c("element",
                             aesthetic,
                             setdiff(c("x", "y"), aesthetic),
                             "id.lengths"))
    list(grob = do.call(element_grob, args), size = size)
}

# Helper for labels in draw function
setup_axis_labels <- function(
    major, minor = NULL, major_pos, minor_pos = NULL,
    position, theme, check.overlap = FALSE, angle = NULL, n.dodge = 1
) {
    aesthetic <- if (position %in% c("top", "bottom")) "x" else "y"
    vertical <- position %in% c("left", "right")

    label_element_name <- paste0("axis.text.", aesthetic, ".", position)
    element <- calc_element(label_element_name, theme)

    # Validate labels if necessary
    major <- validate_labels(major)
    minor <- validate_labels(minor)

    # Override theme defaults with guide specifics
    if (inherits(element, "element_text")) {
        overrides <- .int$axis_label_element_overrides(position, angle)
        element$angle <- overrides$angle %||% element$angle
        element$hjust <- overrides$hjust %||% element$hjust
        element$vjust <- overrides$vjust %||% element$vjust
    }

    # Setup dodging
    n_breaks <- length(minor_pos)
    dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
    dodge_indices <- split(seq_len(n_breaks), dodge_pos)

    # Do minor labels
    label_grobs <- lapply(dodge_indices, function(indices) {
        .int$draw_axis_labels(
            break_positions = minor_pos[indices], break_labels = minor[indices],
            label_element = element,
            is_vertical = vertical, check.overlap = check.overlap
        )
    })
    # Do major labels
    label_grobs <- append(
        label_grobs, list(
            .int$draw_axis_labels(
                break_positions = major_pos, break_labels = major,
                label_element = element,
                is_vertical = vertical, check.overlap = check.overlap
            )
        )
    )
    label_grobs
}

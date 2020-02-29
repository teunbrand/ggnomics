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
  breaks <- c(majorbreaks, minorbreaks)
  lens <- c(length(majorbreaks), length(minorbreaks))

  # Make a data.frame for empty ticks
  empty_ticks <- .int$new_data_frame(
    list(aesthetic = numeric(), .value = numeric(),
         .label = character(), .type = character())
  )
  names(empty_ticks) <- c(aesthetic, ".value", ".label", ".type")

  if (length(intersect(scale$aesthetics, guide$available_aes)) == 0) {
    warning("genomic_axis guide needs appropriate scales: ", 
            guide$available_aes)
    guide$key <- empty_ticks
  } else if (length(breaks) == 0) {
    guide$key <- empty_ticks
  } else {
    mapped_breaks <- if(scale$is_discrete()) scale$map(breaks) else breaks
    ticks <- .int$new_data_frame(setNames(list(mapped_breaks), aesthetic))
    ticks$.value <- breaks
    ticks$.type  <- rep.int(c("major", "minor"), times = lens)
    ticks$.label <- c(scale$get_labels(majorbreaks),
                      scale$get_labels_minor(minorbreaks))

    if (is.list(ticks$.label)) {
      if (any(sapply(ticks$.label, is.language))) {
        ticks$.label <- base::do.call(expression, ticks$.label)
      } else {
        ticks$.label <- unlist(ticks$.label)
      }
    }
    guide$key <- ticks[is.finite(ticks[[aesthetic]]), ]
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
  } else {
    other_aesthetic <- setdiff(c("x", "y"), aesthetics)
    override_value <- if (guide$position %in% c("bottom", "left")) -Inf else Inf
    guide$key[[other_aesthetic]] <- override_value
    guide$key <- coord$transform(guide$key, panel_params)
    .int$warn_for_guide_position(guide)
  }
  key <- guide$key
  key$.value <- Nightfall(key$.value)
  
  # Merge major keys to center over chromosome
  minor <- key[key$.type == "minor", ]
  major <- key[key$.type == "major", ]
  aa <- split(major[aesthetics], factor(major$.label,
                                        levels = unique(major$.label)))
  aa <- matrix(vapply(aa, colMeans, numeric(length(aesthetics)), USE.NAMES = F),
               ncol = length(aesthetics))
  aa <- lapply(seq_along(aesthetics), function(i){aa[,i]})
  major <- major[!duplicated(major$.label), ]
  major[aesthetics] <- aa
  
  # Recombine
  key <- mapply("c", minor, major)
  class(key) <- "data.frame"
  attr(key, "row.names") <- seq_len(nrow(major) + nrow(minor))
  guide$key <- key
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
    break_labels = guide$key$.label,
    break_types = guide$key$.type,
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
  break_labels,
  break_types,
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
  
  # Resolve elements
  line_element_name <- paste0("axis.line.", aesthetic, ".", axis_position)
  tick_element_name <- paste0("axis.ticks.", aesthetic, ".", axis_position)
  tick_length_element_name <- paste0(
    "axis.ticks.length.", aesthetic, ".", axis_position
  )
  label_element_name <- paste0("axis.text.", aesthetic, ".", axis_position)

  # Calculate elements
  line_element <- calc_element(line_element_name, theme)
  tick_element <- calc_element(tick_element_name, theme)
  tick_length  <- calc_element(tick_length_element_name, theme)
  label_element<- calc_element(label_element_name, theme)

  # Override theme defaults with guide specifics
  if (inherits(label_element, "element_text")) {
    label_overrides <- .int$axis_label_element_overrides(axis_position, angle)
    if (!is.null(label_overrides$angle)) {
      label_element$angle <- label_overrides$angle
    }
    if (!is.null(label_overrides$hjust)) {
      label_element$hjust <- label_overrides$hjust
    }
    if (!is.null(label_overrides$vjust)) {
      label_element$vjust <- label_overrides$vjust
    }
  }

  # Do vertical vs horizontal
  is_vertical <- axis_position %in% c("left", "right")
  if (is_vertical) {
    position_dim <- "y"
    non_position_dim <- "x"
    position_size <- "height"
    non_position_size <- "width"
    gtable_element <- gtable::gtable_row
    measure_gtable <- gtable::gtable_width
    measure_labels_non_pos <- grid::grobWidth
  } else {
    position_dim <- "x"
    non_position_dim <- "y"
    position_size <- "width"
    non_position_size <- "height"
    gtable_element <- gtable::gtable_col
    measure_gtable <- gtable::gtable_width
    measure_labels_non_pos <- grid::grobHeight
  }

  # Do primary vs secondary
  is_second <- axis_position %in% c("right", "top")
  if (is_second) {
    tick_direction <- 1
    non_position_panel <- unit(0, "npc")
    tick_coordinate_order <- c(2, 1)
  } else {
    tick_direction <- -1
    non_position_panel <- unit(1, "npc")
    tick_coordinate_order <- c(1, 2)
  }

  # Build axis line
  line_grob <- base::do.call(
    element_grob,
    setNames(
      list(
        line_element, 
        unit(c(0, 1), "npc"),
        grid::unit.c(non_position_panel, non_position_panel)
      ),
      c("element", position_dim, non_position_dim)
    )
  )
  
  # Setup breaks
  is_minor <- break_types == "minor"
  n_breaks <- sum(is_minor)
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

  # Evaluate labels if necessary
  if (is.list(break_labels)) {
    if (any(vapply(break_labels, is.language, logical(1)))){
      break_labels <- base::do.call(expression, break_labels)
    } else {
      break_labels <- unlist(break_labels)
    }
  }

  # Setup dodging
  dodge_pos <- rep(seq_len(n.dodge), length.out = n_breaks)
  dodge_indices <- split(seq_len(n_breaks), dodge_pos)
  
  # Axis labels
  is_minor <- break_types == "minor"
  # Do minor labels
  label_grobs <- lapply(dodge_indices, function(indices) {
    .int$draw_axis_labels(
      break_positions = break_positions[is_minor][indices],
      break_labels = break_labels[is_minor][indices],
      label_element = label_element,
      is_vertical = is_vertical,
      check.overlap = check.overlap
    )
  })
  # Do major labels
  label_grobs <- append(
    label_grobs, list(`2` =
      .int$draw_axis_labels(
        break_positions = break_positions[!is_minor],
        break_labels = break_labels[!is_minor],
        label_element = label_element,
        is_vertical = is_vertical,
        check.overlap = check.overlap
      )
    )
  )

  # Setup tickmarks
  ticks_grob <- base::do.call(
    element_grob,
    setNames(
      list(
        tick_element, 
        rep(unit(break_positions[is_minor], "native"), each = 2),
        rep(grid::unit.c(non_position_panel + (tick_direction * tick_length),
                         non_position_panel)[tick_coordinate_order],
            times = n_breaks), 
        rep(2, times = n_breaks)
      ), 
      c("element", position_dim, non_position_dim, "id.lengths"))
  )
  
  # Combine ticks and labels
  non_position_sizes <- paste0(non_position_size, "s")
  label_dims <- base::do.call(grid::unit.c, lapply(label_grobs, 
                                                   measure_labels_non_pos))
  grobs <- c(list(ticks_grob), label_grobs)
  grob_dims <- grid::unit.c(tick_length, label_dims)

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
             c(non_position_dim, non_position_size, "just"))
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

# Constructors ------------------------------------------------------------

# Variant on ggplot2:::view_scales_from_scale
# It does nothing different besides calling expand_scale_limits_S4 instead of
# ggplot2:::expand_limits_scale
view_scales_from_scale_S4 <- function(scale, coord_limits = NULL, expand = TRUE) {
  # Setup scale expansion
  expansion <- ggplot2:::default_expansion(scale, expand = expand)
  limits <- scale$get_limits()
  continuous_range <- expand_scale_limits_S4(scale,
                                             expansion,
                                             limits,
                                             coord_limits = coord_limits)
  aesthetic <- scale$aesthetics[1]

  view_scales <- list(
    view_scale_primaryS4(scale, limits, continuous_range),
    sec = ggplot2:::view_scale_secondary(scale, limits, continuous_range),
    arrange = scale$axis_order(),
    range = continuous_range
  )
  names(view_scales) <- c(aesthetic,
                          paste0(aesthetic, ".", names(view_scales)[-1]))
  view_scales
}

# TODO test with discrete scales
# Does the same job as ggplot2:::expand_limits_scale.
# Difference is that undefined coord limits are adapted to the length of the
# limits. Furthermore, doesn't try to coerce the coord limits to other classes
# with the scale's transform. Also calls expand_limits_continuous_S4 instead of
# ggplot2:::expand_limits_continuous.
expand_scale_limits_S4 <- function(scale, expand = expansion(0, 0),
                                   limits = waiver(), coord_limits = NULL) {
  limits <- if (!inherits(limits, "waiver")) {
    limits
  } else {
    scale$get_limits()
  }
  if (is.null(coord_limits)) {
    coord_limits <- rep(NA_real_, length(limits))
  }

  if (scale$is_discrete()) {
    ggplot2:::expand_limits_discrete(
      limits,
      expand,
      coord_limits,
      range_continuous = scale$range_c$range
    )
  } else {
    expand_limits_continuous_S4(
      limits,
      expand,
      coord_limits
    )$continuous_range
  }
}

# Does pretty much the same as ggplot2:::expand_limits_continuous_trans
expand_limits_continuous_S4 <- function(
  limits, expand = expansion(0, 0), coord_limits = c(NA, NA),
  trans = scales::identity_trans()
) {
  # Override limits with non-NA coord limits
  limits[!is.na(coord_limits)] <- coord_limits

  # From data space to internal space
  continuous_range_coord <- trans$transform(limits)

  # The range expansion expects ordered values, fix for reversed coordinates
  o <- order(continuous_range_coord)
  continuous_range_coord <- continuous_range_coord[o]
  continuous_range_coord <- expand_range5(continuous_range_coord, expand)
  continuous_range_coord <- continuous_range_coord[order(o)]

  # From internal space back to data space
  final_scale_limits <- trans$inverse(continuous_range_coord)

  # Safety measure to catch any case where we end up with non-finite scales
  final_scale_limits[try_is_infinite(final_scale_limits)] <- limits

  list(continuous_range_coord = continuous_range_coord,
       continuous_range = final_scale_limits)
}

# Like ggplot2:::expand_range4
expand_range5 <- function(limits, expand) {
  if (!(is.numeric(expand) && length(expand) %in% c(2L, 4L))) {
    rlang::abort("`expand` must be a numeric vector with 1 or 2 elements")
  }

  if (all(try_is_infinite(limits))) {
    return(c(-Inf, Inf))
  }

  # Safety measure for old syntax
  if (length(expand == 2)) {
    expand <- c(expand, expand)
  }

  # Delegate class specific responsibilities to S4 method
  S4ExpandRange(limits, expand)
}

# Does the same thing as ggplot2:::view_scale_primary but have the viewscale
# inherit from ViewScaleS4
view_scale_primaryS4 <- function(
  scale, limits = scale$get_limits(),
  continuous_range = self$dimension(limits = limits))
{
  if (!scale$is_discrete()) {
    breaks <- scale$get_breaks(continuous_range)
    minor_breaks <- scale$get_breaks_minor(b = breaks,
                                           limits = continuous_range)
  } else {
    breaks <- scale$get_breaks(limits)
    minor_breaks <- scale$get_breaks_minor(breaks, limits = limits)
  }

  ggproto(NULL, ViewScaleS4,
          scale = scale,
          guide = scale$guide,
          position = scale$position,
          aesthetics = scale$aesthetics,
          name = scale$name,
          scale_is_discrete = scale$is_discrete(),
          limits = limits,
          continuous_range = continuous_range,
          breaks = breaks,
          minor_breaks = minor_breaks)
}

# ggproto -----------------------------------------------------------------

#' @describeIn ggnomics_extensions An child to ggplot's ViewScale ggproto that
#'   has a minor label getter. Note: this class is not exported.
#' @usage NULL
#' @format NULL
ViewScaleS4 <- ggproto(
  "ViewScaleS4",
  ggplot2:::ViewScale,
  get_label_minor = function(self, breaks = self$get_breaks_minor()) {
    self$scale$get_labels_minor(breaks)
  }
)

# Helpers -----------------------------------------------------------------

# Some S4 classes cannot be finite so lack an is.finite method
# TODO: check wether this is redundant with utils.R/check_finite
try_is_infinite <- function(x) {
  tryCatch({!is.finite(x)},
           error = function(x) {rep(FALSE, length(x))})
}

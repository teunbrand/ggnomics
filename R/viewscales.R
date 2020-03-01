# Constructors ------------------------------------------------------------

# Variant on ggplot2:::view_scales_from_scale
# It does nothing different besides calling expand_scale_limits_S4 instead of
# ggplot2:::expand_limits_scale
view_scales_from_scale_S4 <- function(scale, coord_limits = NULL, expand = TRUE) {

  # Setup scale expansion
  expansion <- .int$default_expansion(scale, expand = expand)
  limits <- scale$get_limits()
  continuous_range <- expand_scale_limits_S4(scale,
                                             expansion,
                                             limits,
                                             coord_limits = coord_limits)
  aesthetic <- scale$aesthetics[1]

  primary <- view_scale_primaryS4(scale, limits, continuous_range)
  view_scales <- list(
    primary,
    sec = view_scale_secondaryS4(scale, limits, continuous_range,
                                 prototype = primary),
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
    .int$expand_limits_discrete(
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

# Has extra prototype argument to avoid reconstructing the primary when 
# appropriate
view_scale_secondaryS4 <- function(
  scale, limits = scale$get_limits(), 
  continuous_range = scale$dimension(limits = limits),
  prototype = NULL
) {
  if (is.null(scale$secondary.axis) || 
      inherits(scale$secondary.axis, "waiver") || 
      scale$secondary.axis$empty()) {
    # If there is no second axis, return primary scale with no guide
    # this guide can be overriden using guides()
    if (is.null(prototype)) {
      primary_scale <- view_scale_primaryS4(scale, limits, continuous_range)
    } else {
      primary_scale <- ggproto(NULL, prototype)
    }
    .int$scale_flip_position(primary_scale)
    primary_scale$guide <- guide_none()
    primary_scale
  } else {
    scale$secondary.axis$init(scale)
    break_info <- scale$secondary.axis$break_info(continuous_range, scale)
    names(break_info) <- gsub("sec\\.", "", names(break_info))
    
    # flip position from the original scale by default
    # this can (should) be overriden in the guide
    position <- switch(
      scale$position,
      top = "bottom",
      bottom = "top",
      left = "right",
      right = "left",
      scale$position
    )
    
    ggproto(
      NULL,
      ViewScaleS4Secondary,
      scale = scale,
      guide = scale$secondary.axis$guide,
      position = position,
      break_info = break_info,
      aesthetics = scale$aesthetics,
      name = scale$sec_name()
    )
  }
}

# ggproto -----------------------------------------------------------------

#' @describeIn ggnomics_extensions An child to ggplot's ViewScale ggproto that
#'   has a minor label getter. Note: this class is not exported.
#' @usage NULL
#' @format NULL
ViewScaleS4 <- ggproto(
  "ViewScaleS4",
  ggplot2:::ViewScale,
  get_labels_minor = function(self, breaks = self$get_breaks_minor()) {
    self$scale$get_labels_minor(breaks)
  }
)

#' @describeIn ggnomics_extensions A child to ViewScaleS4 that has static break
#'   information. Note: this class is not exported.
#' @usage NULL
#' @format NULL
ViewScaleS4Secondary <- ggproto(
  "ViewScaleS4Secondary",
  ViewScaleS4,
  make_title = function(self, title) self$scale$make_sec_title(title),
  dimension = function(self) self$break_info$range,
  get_limits = function(self) self$break_info$range,
  get_breaks = function(self) self$break_info$major_source,
  break_positions = function(self) self$break_info$major,
  break_positions_minor = function(self) self$break_info$minor,
  get_labels = function(self, breaks = self$get_breaks()) {
    self$break_info$labels
  },
  rescale = function(x) S4Rescale(x, from = break_info$range, to = c(0, 1))
)

# Helpers -----------------------------------------------------------------

# Some S4 classes cannot be finite so lack an is.finite method
# TODO: check wether this is redundant with utils.R/check_finite
try_is_infinite <- function(x) {
  tryCatch({!is.finite(x)},
           error = function(x) {rep(FALSE, length(x))})
}

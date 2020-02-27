#' @name scale_S4_continuous
#'
#' @title Position scales for S4 continuous data (x & y)
#'
#' @description \code{scale_x_S4_continuous} and \code{scale_y_S4_continuous}
#' are the analogues of \code{\link[ggplot]{scale_x_continuous}} and
#'  \code{\link[ggplot]{scale_y_continuous}}. They are the default scales for S4
#'  data classes.
#'
#' @inheritParams ggplot2::scale_x_continuous
#' @param minor_labels One of:
#'   \itemize{
#'    \item \code{NULL} for no minor labels.
#'    \item \code{waiver()} for the default labels computed by the
#'    transformation object.
#'    \item A \code{character} vector giving labels (must be same length as
#'    \code{minor_breaks}.)
#'    \item A \code{function} that takes the minor breaks as input and returns
#'    labels as output.
#'   }
#'   Keep in mind that displaying the minor labels is dependent on wether the
#'   \code{guide} supports this and the default \code{guide_axis()} does not.
#'
#' @note Note that by default, setting limits on positional
#'   scales will \strong{remove} data outside of the limits. Change the
#'   \code{oob} argument to change this behaviour.
#'
#' @export
#'
#' @examples
#' NULL
#'

#' @export
#' @rdname scale_S4_continuous
scale_x_S4_continuous <- function(
  name   = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  minor_labels = waiver(),
  limits = NULL,
  expand = waiver(),
  oob = censorThis,
  na.value = NA_real_,
  trans = S4TransIdentity,
  guide = waiver(),
  position = "bottom",
  sec.axis = waiver()
) {
  sc <- S4_continuous_scale(
    aesthetics = c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
                   "xmax_final", "xlower", "xmiddle", "xupper", "x0", "xrange"),
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    minor_labels = minor_labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    super = ScaleS4ContinuousPosition
  )
  ggplot2:::set_sec_axis(sec.axis, sc)
}

#' @export
#' @rdname scale_S4_continuous
scale_y_S4_continuous <- function(
  name   = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  minor_labels = waiver(),
  labels = waiver(),
  limits = NULL,
  expand = waiver(),
  oob = censorThis,
  na.value = NA_real_,
  trans = S4TransIdentity,
  guide = waiver(),
  position = "left",
  sec.axis = waiver()
) {
  sc <- S4_continuous_scale(
    aesthetics = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
                   "ymax_final", "lower", "middle", "upper", "y0", "yrange"),
    scale_name = "position_c",
    palette = identity,
    name = name,
    breaks = breaks,
    n.breaks = n.breaks,
    minor_breaks = minor_breaks,
    labels = labels,
    minor_labels = minor_labels,
    limits = limits,
    expand = expand,
    oob = oob,
    na.value = na.value,
    trans = trans,
    guide = guide,
    position = position,
    super = ScaleS4ContinuousPosition
  )
  ggplot2:::set_sec_axis(sec.axis, sc)
}

# Internal Constructor ----------------------------------------------------

S4_continuous_scale <- function(
  aesthetics,
  scale_name,
  palette,
  name = waiver(),
  breaks = waiver(),
  minor_breaks = waiver(),
  n.breaks = NULL,
  labels = waiver(),
  minor_labels = waiver(),
  limits = NULL,
  rescaler = S4Rescale,
  oob = censorThis,
  expand = waiver(),
  na.value = NA_real_,
  trans = S4TransIdentity,
  guide = "legend",
  position = "left",
  super = ScaleS4ContinuousPosition
) {
  aesthetics <- standardise_aes_names(aesthetics)
  
  ggplot2:::check_breaks_labels(breaks, labels)
  ggplot2:::check_breaks_labels(minor_breaks, minor_labels)
  
  position <- match.arg(position, c("left", "right", "top", "bottom"))
  
  if (is.null(breaks) && all(!ggplot2:::is_position_aes(aesthetics))) {
    guide <- "none"
  }
  
  trans <- scales::as.trans(trans)
  if (!is.null(limits) && !is.function(limits)) {
    limits <- trans$transform(limits)
  }
  
  ggproto(
    NULL, super,
    call = match.call(),
    aesthetics = aesthetics,
    scale_name = scale_name,
    palette = palette,
    range = new_S4_continuous_range(aesthetics[1]),
    limits = limits,
    trans = trans,
    na.value = na.value,
    expand = expand,
    rescaler = rescaler,
    oob = oob,
    name = name,
    breaks = GreekSoldier(breaks),
    minor_breaks = GreekSoldier(minor_breaks),
    n.breaks = n.breaks,
    labels = labels,
    minor_labels = minor_labels,
    guide = guide,
    position = position
  )
}
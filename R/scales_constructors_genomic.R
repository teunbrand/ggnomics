#' @name scale_genomic
#' @title Position scales for genomic data (x & y)
#' @aliases scale_x_genomic scale_y_genomic scale_genomic
#'
#' @description \code{scale_x_genomic()} and \code{scale_y_genomic()} are the
#'   default scales for x and y aesthetics defined as representations of genomic
#'   positions such as \linkS4class{GPos} and \linkS4class{GRanges} (sub)-classes.
#'
#' @inheritParams scale_S4_continuous
#' @param limits One of:
#'   \itemize{
#'    \item \code{NULL} to use the default scale range .
#'    \item A \code{GRanges} object with at most one range per seqlevel to be
#'    displayed.
#'    \item A function that accepts the existing (automatic) limits as
#'    \code{GRanges} object and returns the new limits as a \code{GRanges}
#'    object.
#'   }
#'   Note that by default, setting limits on positional
#'   scales will \strong{remove} data outside of the limits. Change the
#'   \code{oob} argument to change this behaviour.
#' @param breaks One of:
#'   \itemize{
#'    \item \code{NULL} for no breaks
#'    \item \code{waiver()} for the default breaks computed by the
#'    transformation object.
#'    \item A \code{GPos} object of positions.
#'    \item A function that takes the \code{GRanges} limits and returns
#'    \code{GPos} breaks as output.
#'   }
#' @param minor_breaks One of:
#'   \itemize{
#'    \item \code{NULL} for no minor breaks
#'    \item \code{waiver()} for the default minor breaks.
#'    \item A \code{GPos} object of positions.
#'    \item A function that takes the \code{GRanges} limits and returns
#'    \code{GPos} minor breaks as output.
#'   }
#' @details It is important to note that the scale operates in genomic space
#'   when limits are defined as or inferred to be of the \code{GRanges} class.
#'   This has the following known consequences:
#'   \enumerate{
#'    \item Data represented by unclassed numerical values can not be used on
#'    this scale.
#'    \item The scale limits and breaks are represented by \code{GRanges} and
#'    \code{GPos} objects.
#'   }
#'   Furthermore, scale expansions as set through the \code{expand} argument,
#'   expand the scale limits per individual \code{seqlevel}.
#'
#' @seealso The \code{\link[GenomicRanges]{GRanges}} class and
#'   \code{\link[GenomicRanges]{GPos}} class.
#'
#' @examples
#' require(GenomicRanges)
#'
#' df <- DataFrame(
#'   x = GRanges(c("chr1:1000-2000", "chr1:1500-2500",
#'                 "chr2:3000-4000", "chr3:4000-4500")),
#'   y = c(1, 2, 3, 2)
#' )
#'
#' # The default scale for GenomicRanges is scale_(x|y)_genomic()
#' g <- ggplot(df, aes(xmin = x, xmax = x,
#'                     ymin = y - 0.2, ymax = y + 0.2)) +
#'   geom_rect()
#' g
#'
#' # Major labels are parallel to the seqnames
#' g + scale_x_genomic(labels = LETTERS[1:3])
#'
#' # Minor breaks are positional and specified with GPos or 1-width GRanges
#' g + scale_x_genomic(
#'   minor_breaks =  GPos(c("chr1:2000", "chr2:3500", "chr3:4000"))
#' )
#'
#' # Minor breaks can also be labelled
#' g + scale_x_genomic(
#'   minor_breaks =  GPos(c("chr1:2000", "chr2:3500", "chr3:4000")),
#'   minor_labels = LETTERS[1:3]
#' )

#' @rdname scale_genomic
#' @export
scale_x_genomic <- function(
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
  guide = "genomic_axis",
  position = "bottom",
  sec.axis = waiver()
) {
  sc <- S4_continuous_scale(
    aesthetics = c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
                   "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
    scale_name = "genomic",
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
    super = ScaleGenomic
  )
  .int$set_sec_axis(sec.axis, sc)
}

#' @rdname scale_genomic
#' @export
scale_y_genomic <- function(
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
  guide = "genomic_axis",
  position = "left",
  sec.axis = waiver()
) {
  sc <- S4_continuous_scale(
    aesthetics = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
                   "ymax_final", "lower", "middle", "upper", "y0"),
    scale_name = "genomic",
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
    super = ScaleGenomic
  )
  .int$set_sec_axis(sec.axis, sc)
}
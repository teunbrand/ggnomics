# Main function ----------------------------------------------------------------

#' Create a layer as track
#'
#' \code{tracklayer} moves the responsibilities of setting facet specifications
#' from the facet function to the layer, which allows defining facets at the
#' layer level.
#'
#' @inheritParams ggplot2::layer
#' @param track_y A \code{character} of length 1 setting the name of a facet row
#'   for this layer. Alternatively, \code{track_y} may also be defined in the
#'   aesthetics mapping.
#' @param track_x A \code{character} of length 1 setting the name of a facet
#'   column for this layer. Alternatively, \code{track_x} may also be defined in
#'   the aesthetics mapping.
#' @param ... Other arguments passed on to the geom, stat or position
#'   components. These are often aesthetics, used to set an aesthetic to a fixed
#'   value, like \code{colour = "red"} or \code{size = 3}. They may also be
#'   parameters to the paired geom/stat.
#'
#' @details There are two behaviours of \code{tracklayer} layers which are
#'   different from the regular \code{layer} function.
#'
#'   First, before facet are evaluated, \code{tracklayer} attaches the
#'   \code{"track_y"} and \code{"track_x"} columns to the data, allowing these
#'   to be found by the facet functions. These can either be provided as
#'   arguments to the layer or within aesthetic mappings; the latter will be
#'   prioritised over the former.
#'
#'   Secondly, when a \code{tracklayer} is added to a plot, it will check if the
#'   plot already has non-default facets. If the plot has the default facet
#'   (i.e. as produced by \code{facet_null()}), this facet is replaced by
#'   \code{facet_track} that only takes \code{track_x} and \code{track_y} as
#'   facets.
#'
#' @seealso \code{\link[ggnomics]{facet_track}} for the sister function in
#'   laying out tracked facets. \code{\link[ggplot2]{layer}} for the
#'   conventional use of layers.
#'
#' @export
#'
#' @examples
#' # Tracked layer as argument
#' ggplot() +
#'   tracklayer(mapping = aes(wt, drat),
#'              data = mtcars, track_y = "mtcars") +
#'   tracklayer(mapping = aes(Sepal.Width, Sepal.Length, colour = Species),
#'              data = iris, track_y = "iris")
#'
#' # Tracked layer through aesthetics mapping
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   tracklayer(mapping = aes(colour = Species, track_y = Species))
#'   
#' # 'track_y' and 'track_x' can also be used with conventional facets
#' ggplot(iris, aes(Sepal.Width, Sepal.Length)) +
#'   tracklayer(mapping = aes(colour = Species, track_x = Species)) +
#'   facet_wrap(~ track_x)
tracklayer <- function(data = NULL,
                       mapping = NULL,
                       geom = "point",
                       stat = "identity",
                       position = "identity",
                       inherit.aes = TRUE,
                       check.aes = TRUE,
                       check.param = TRUE,
                       show.legend = NA,
                       key_glyph = NULL,
                       track_y = "Default",
                       track_x = "Default",
                       ...) {
  params <- list(...)
  if (is.null(geom)) {
    stop("Attempted to create layer with no geom.", call. = FALSE)
  }
  if (is.null(stat)) {
    stop("Attempted to create layer with no stat.", call. = FALSE)
  }
  if (is.null(position)) {
    stop("Attempted to create layer with no position", call. = FALSE)
  }
  if (!is.logical(show.legend)) {
    warning("`show.legend` must be a logical vector.", call. = FALSE)
    show.legend <- FALSE
  }
  if (!is.null(mapping)) {
    mapping <- .int$validate_mapping(mapping)
  }
  data <- fortify(data)
  geom <- .int$check_subclass(geom, "Geom", env = parent.frame())
  stat <- .int$check_subclass(stat, "Stat", env = parent.frame())
  position <- .int$check_subclass(position, "Position", env = parent.frame())
  if (is.null(params$na.rm)) {
    params$na.rm <- FALSE
  }
  if (!is.null(params$key_glyph)) {
    params$na.rm <- FALSE
  }
  if (!is.null(params$key_glyph)) {
    key_glyph <- params$key_glyph
    params$key_glyph <- NULL
  }
  params <- .int$rename_aes(params)
  aes_params  <- params[intersect(names(params), geom$aesthetics())]
  geom_params <- params[intersect(names(params), geom$parameters(TRUE))]
  stat_params <- params[intersect(names(params), stat$parameters(TRUE))]
  all <- c(geom$parameters(TRUE), stat$parameters(TRUE), geom$aesthetics())
  extra_param <- setdiff(names(params), all)
  if (check.param && length(extra_param) > 0) {
    warning("Ignoring unknown parameters: ", paste(extra_param, collapse = ", "),
            call. = FALSE, immediate. = TRUE)
  }
  extra_aes <- setdiff(.int$mapped_aesthetics(mapping),
                       c(geom$aesthetics(), stat$aesthetics(),
                         "track_x", "track_y"))
  if (check.aes && length(extra_aes) > 0) {
    warning("Ignoring unknown aesthetics: ", paste(extra_aes,
                                                   collapse = ", "),
            call. = FALSE, immediate. = TRUE)
  }
  geom <- .int$set_draw_key(geom, key_glyph)
  ggproto("TrackLayerInstance", TrackLayer, geom = geom, 
          geom_params = geom_params,
          stat = stat, stat_params = stat_params, data = data,
          mapping = mapping, aes_params  = aes_params, position = position,
          track_y = track_y, track_x = track_x,
          inherit.aes = inherit.aes, show.legend = show.legend)
}

# ggproto -----------------------------------------------------------------

#' @usage NULL
#' @format NULL
#' @export
#' @rdname ggnomics_extensions
TrackLayer <- ggproto(
  "TrackLayer", ggplot2:::Layer,
  setup_layer = function(self, data, plot) {
    data <- ggproto_parent(ggplot2:::Layer, self)$setup_layer(data, plot)
    
    if (self$inherit.aes) {
      aesthetics <- .int$defaults(self$mapping, plot$mapping)
    } else {
      aesthetics <- self$mapping
    }
    
    has_x <- "track_x" %in% names(aesthetics)
    has_y <- "track_y" %in% names(aesthetics)
    
    if (has_x) {
      data$track_x <- rlang::eval_tidy(aesthetics[["track_x"]], data = data)
    } else {
      data$track_x <- self$track_x
    }
    
    if (has_y) {
      data$track_y <- rlang::eval_tidy(aesthetics[["track_y"]], data = data)
    } else {
      data$track_y <- self$track_y
    }
    
    data
  }
)

# Helpers -----------------------------------------------------------------

#' @export
ggplot_add.TrackLayer <- function(object, plot, object_name) {
  if (inherits(plot$facet, "FacetNull")) {
    plot <- plot + facet_track()
  }
  plot$layers <- append(plot$layers, object)
  mapping <- .int$make_labels(object$mapping)
  default <- .int$make_labels(object$stat$default_aes)
  new_labels <- .int$defaults(mapping, default)
  plot$labels <- .int$defaults(plot$labels, new_labels)
  plot
}

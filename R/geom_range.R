#' Ranges
#'
#' Alternative parameterisation of rectangles which can take a
#' \code{Ranges}-class, such as \linkS4class{IRanges} or \linkS4class{GRanges}
#' as x or y argument.
#'
#' @inheritParams ggplot2::geom_rect
#'
#' @return A \code{Layer} ggproto object.
#'
#' @export
#'
#' @details If the \code{x} position aesthetic is given while \code{y} is not,
#' and vice versa, the default behaviour is to center the aesthetic that is not
#' provided around zero with a height/width of 0.8. If the position aesthetic is
#' \code{numeric}, it is expected that also the \code{(x|y)end} aesthetic is
#' provided.
#'
#' @section Aesthetics: \code{geom_range} understands the following
#' aesthetics (required aesthetics are in bold, optional in italic).
#' \itemize{
#'  \item \strong{x}
#'  \item \strong{y}
#'  \item \emph{xend} (if data is \code{numeric})
#'  \item \emph{yend} (if data is \code{numeric})
#'  \item alpha
#'  \item colour
#'  \item fill
#'  \item group
#'  \item linetype
#'  \item size
#' }
#'
#' @examples
#' require(IRanges)
#' df <- DataFrame(x = IRanges(c(1, 2), c(3, 4)))
#'
#' ggplot(df) +
#'   geom_range(aes(x))
geom_range <- function(
    mapping = NULL,
    data = NULL,
    stat = "identity",
    position = "identity",
    ...,
    linejoin = "mitre",
    na.rm = FALSE, show.legend = NA,
    inherit.aes = TRUE
) {
    layer(
        data = data,
        mapping = mapping,
        stat = stat,
        geom = GeomRange,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(linejoin = linejoin, na.rm = na.rm, ...)
    )
}


#' @export
#' @usage NULL
#' @describeIn ggnomics_extensions A child to GeomRects. See
#'   \code{\link[ggnomics]{geom_range}}.
#' @format NULL
GeomRange <- ggproto(
    "GeomRange",
    GeomRect,
    required_aes = "x|y",
    optional_aes = "xend|yend",
    default_aes = aes(colour = NA, fill = "grey35", size = 0.5, linetype =1,
                      alpha = NA),
    aesthetics = function(self) {
        if (is.null(self$required_aes)) {
            required_aes <- NULL
        } else {
            required_aes <- .flatstrsplit(self$required_aes, "|")
        }
        if (is.null(self$option_aes)) {
            optional_aes <- NULL
        } else {
            optional_aes <- .flatstrsplit(self$optional_aes, "|")
        }
        c(union(union(required_aes, names(self$default_aes)), optional_aes),
          "group")
    },
    setup_data = function(data, params) {
        if (is.null(data$x)) {
            data$x <- -0.4
        }
        if (is.numeric(data$x)) {
            data$xend <- data$xend %||% (data$x + 0.8)
        }
        if (is.null(data$y)) {
            data$y <- -0.4
        }
        if (is.numeric(data$y)) {
            data$yend <- data$yend %||% (data$y + 0.8)
        }
        if (HelenOfTroy(data$x, "Ranges")) {
            data$xend <- NULL
        }
        if (HelenOfTroy(data$y, "Ranges")) {
            data$yend <- NULL
        }
        data
    },
    draw_panel = function(self, data, panel_params, coord, linejoin = "mitre") {
        if (HelenOfTroy(data$x, "Ranges")) {
            data <- transform(data, xmin = x, xmax = x, x = NULL)
        } else {
            data <- transform(data, xmin = x, xmax = xend, x = NULL)
        }
        if (HelenOfTroy(data$y, "Ranges")) {
            data <- transform(data, ymin = y, ymax = y, y = NULL)
        } else {
            data <- transform(data, ymin = y, ymax = yend)
        }
        ggproto_parent(GeomRect, self)$draw_panel(
            data = data, panel_params = panel_params, coord = coord,
            linejoin = "mitre"
        )
    }
)

# Helpers -----------------------------------------------------------------

.flatstrsplit <- function(x, split) {
    unlist(strsplit(x, split, fixed = TRUE))
}

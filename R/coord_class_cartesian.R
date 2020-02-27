#' @include viewscales.R
NULL

# ggproto -----------------------------------------------------------------

#' @describeIn ggnomics_extensions See \code{\link[ggnomics]{coord_S4}}
#' @usage NULL
#' @format NULL
CoordS4 <- ggproto(
  "CoordS4",
  CoordCartesian,
  setup_data = function(data, params = list()) {
    # This is a (hopefully )temporary workaround to protect S4 classes from
    # contraints on aesthetic evaluation, i.e. the rlang::is_vector and
    # tibble::as_tibble validation of column data.
    data <- lapply(data, function(layer_data) {
      layer_data[] <- lapply(layer_data, GreekSoldier)
      layer_data
    })
    data
  },
  transform = function(data, panel_params) {
    if (!inherits(panel_params$x$scale, "ScaleS4") &&
        !inherits(panel_params$y$scale, "ScaleS4")) {
      return(CoordCartesian$transform(data, panel_params))
    }
    oldclass <- class(data)
    data <- unclass(data)
    scales <- ggplot2:::aes_to_scale(names(data))
    
    is_x <- scales == "x"
    xtypes <- names(data)[is_x]
    
    is_y <- scales == "y"
    ytypes <- names(data)[is_y]
    
    if (inherits(panel_params$x$scale, "ScaleS4")) {
      trans_x <- panel_params$x$scale$final_transformer
      if (!is.null(trans_x)) {
        data[is_x] <- mapply(
          trans_x, x = data[is_x], aes = xtypes,
          MoreArgs = list(limits = panel_params$x$continuous_range),
          SIMPLIFY = FALSE
        )
      }
    } else {
      trans_x <- panel_params$x$rescale
      if (!is.null(trans_x)) {
        data[is_x] <- lapply(data[is_x], trans_x)
      }
    }
    
    if (inherits(panel_params$y$scale, "ScaleS4")) {
      trans_y <- panel_params$y$scale$final_transformer
      if (!is.null(trans_y)) {
        data[is_y] <- mapply(
          trans_y, x = data[is_y], aes = ytypes,
          MoreArgs = list(limits = panel_params$y$continuous_range),
          SIMPLIFY = FALSE
        )
      }
    } else {
      trans_y <- panel_params$y$rescale
      if (!is.null(trans_y)) {
        data[is_y] <- lapply(data[is_y], trans_y)
      }
    }
    
    class(data) <- oldclass
    # At this point the data should be ready to be fed into drawing functions
    ggplot2:::transform_position(data,
                                 scales::squish_infinite,
                                 scales::squish_infinite)
  },
  setup_panel_params = function(self, scale_x, scale_y, params = list()) {
    # We want to control how view scales are constructed
    c(
    view_scales_from_scale_S4(scale_x, self$limits$x, self$expand),
    view_scales_from_scale_S4(scale_y, self$limits$y, self$expand)
    )
  }
)

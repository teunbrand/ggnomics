#' @include transforms.R
#' @include utils.R
NULL

# ScaleS4 Parent ggproto --------------------------------------------------

#' @describeIn ggnomics_extensions A child to ggplot's Scale ggproto. Note that
#'   this class is not exported but children are.
#' @usage NULL
#' @format NULL
ScaleS4 <- ggproto(
    "ScaleS4",
    Scale,
    range = ggproto(NULL, RangeS4),
    final_transformer = S4ForceFlat,
    clone = function(self) {
        new <- ggproto(NULL, self)
        new$range <- new_S4_continuous_range(new$aesthetics[1])
        new
    }
)

# ScaleS4Continuous -------------------------------------------------------

#' @describeIn ggnomics_extensions A child to ScaleS4 and sibling to ggplot's
#'   ScaleContinuous.
#' @usage NULL
#' @format NULL
#' @export
ScaleS4Continuous <- ggproto_sibling(
    class_name = "ScaleS4Continuous",
    parent = ScaleS4,
    sister = ScaleContinuous,
    range = new_S4_continuous_range(aes = NULL),
    rescaler = S4Rescale,
    oob = censorThis,
    minor_breaks = waiver(),
    minor_labels = waiver(),
    transform = function(self, x) {
        x <- GreekSoldier(x)
        new_x <- self$trans$transform(x)
        axis <- if ("x" %in% self$aesthetics) "x" else "y"
        .int$check_transformation(x, new_x, self$scale_name, axis)
        new_x
    },
    get_labels_minor = function(self, breaks = self$get_breaks_minor()) {
        if (is.null(breaks)) {
            return(NULL)
        }

        breaks <- self$trans$inverse(breaks)

        if (is.null(self$minor_labels)) {
            return(NULL)
        }

        if (identical(self$minor_labels, NA)) {
            stop("Invalid minor labels specification. Use NULL, not NA")
        }

        if (inherits(self$minor_labels, "waiver")) {
            # See if format has a 'type' argument
            if ("type" %in% names(formals(self$trans$format))) {
                labels <- self$trans$format(breaks, type = "minor")
            } else {
                labels <- self$trans$format(breaks)
            }
        }  else if (is.function(self$minor_labels)) {
            labels <- self$minor_labels(breaks)
        } else {
            labels <- self$minor_labels
        }

        if (length(labels) != length(breaks)) {
            stop("Breaks and labels are different lengths")
        }

        labels
    },
    n.breaks = NULL,
    trans = S4TransIdentity,
    is_empty = function(self) {
        has_data <- !is.null(self$range$range)
        has_limits <- is.function(self$limits) ||
            (!is.null(self$limits) && all(check_finite(self$limits)))
        !has_data && !has_limits
    },
    map = function(self, x, limits = self$get_limits()) {
        x <- self$rescale(self$oob(x, range = limits), limits)

        uniq <- unique(x)
        pal <- self$palette(uniq)
        scaled <- pal[match(x, uniq)]

        setNA(scaled, is.na(scaled))
    },
    get_limits = function(self) {
        if (self$is_empty()) {
            return(c(0, 1))
        }

        lims <- self$limits

        if (is.null(lims)) {
            self$range$range
        } else if (is.function(lims)) {
            self$trans$transform(lims(self$trans$inverse(self$range$range)))
        } else {
            lims[is.na(lims)] <- self$range$range
            lims
        }
    },
    dimension = function(self, expand = expansion(0, 0),
                         limits = self$get_limits()) {
        expand_scale_limits_S4(self, expand, limits)
    },
    get_breaks = function(self, limits = self$get_limits()) {
        if (self$is_empty()) {
            return(numeric())
        }

        # Transform limits back to data space
        limits <- self$trans$inverse(limits)

        if (is.null(self$breaks)) {
            return(NULL)
        }

        if (identical(self$breaks, NA)) {
            abort("Invalid breaks specification. Use NULL, not NA")
        }

        if (S4ZeroRange(limits)) {
            breaks <- limits[1]
        } else if (inherits(self$breaks, "waiver")) {
            if (!is.null(self$n.breaks) &&
                .int$trans_support_nbreaks(self$trans)) {
                breaks <- self$trans$breaks(limits, self$n.breaks)
            } else {
                if (!is.null(self$n.breaks)) {
                    msg <- paste("Ignoring n.breaks. Use a trans object that",
                                 "supports setting number of breaks")
                    rlang::warn(msg)
                }
                breaks <- self$trans$breaks(limits)
            }
        } else if (is.function(self$breaks)) {
            breaks <- self$breaks(limits)
        } else {
            breaks <- self$breaks
        }
        breaks <- GreekSoldier(breaks)

        # Convert data space back to transformed space
        breaks <- self$trans$transform(breaks)
        # Any breaks outside the dimensions are flagged as missing
        breaks <- censorThis(breaks, self$trans$transform(limits),
                             only.finite = FALSE,
                             aes = self$aesthetics[1])
    },
    get_breaks_minor = function(self, n = 2,
                                b = self$break_positions(),
                                limits = self$get_limits()) {
        if (S4ZeroRange(limits)) {
            return()
        }

        if (is.null(self$minor_breaks)) {
            return(NULL)
        }

        if (identical(self$minor_breaks, NA)) {
            msg <- "Invalid minor_breaks specification. Use NULL, not NA."
            rlang::abort(msg)
        }

        if (inherits(self$minor_breaks, "waiver")) {
            if (is.null(b)) {
                breaks <- NULL
            } else {
                breaks <- self$trans$minor_breaks(b, limits, n)
            }
        } else if (is.function(self$minor_breaks)) {
            # Find breaks in data space, convert to numeric
            breaks <- self$minor_breaks(self$trans$inverse(limits))
            breaks <- self$trans$transform(breaks)
        } else {
            breaks <- self$trans$transform(self$minor_breaks)
        }

        GreekSoldier(discardOob(breaks, limits))
    },
    print = function(self, ...) {
        show_range <- function(x) paste0(showAsCell(x), collapse = " -- ")

        cat("<", class(self)[[1]], ">\n", sep = "")
        cat(" Range:  ", show_range(self$range$range), "\n", sep = "")
        cat(" Limits: ", show_range(self$dimension()), "\n", sep = "")
    }
)

# ScaleS4Discrete ---------------------------------------------------------

#' @describeIn ggnomics_extensions A child to ScaleS4 and sibling to ggplot's
#'   ScaleDiscrete
#' @usage NULL
#' @format NULL
#' @export
ScaleS4Discrete <- ggproto_sibling(
    class_name = "ScaleS4Discrete",
    parent = ScaleS4,
    sister = ScaleDiscrete,
    map = function(self, x, limits = self$get_limits()) {
        n <- sum(!is.na(limits))

        # Get palette
        if (!is.null(self$n.breaks.cache) && self$n.breaks.cache == n) {
            pal <- self$palette.cache
        } else {
            if (!is.null(self$n.breaks.cache)) {
                warn("Cached palette does not match requested")
            }
            pal <- self$palette(n)
            self$palette.cache <- pal
            self$n.breaks.cache <- n
        }

        if (rlang::is_named(pal)) {
            idx_nomatch <- is.na(match(names(pal), limits))
            pal[idx_nomatch] <- NA
            pal_match <- pal[match(as.character(Nightfall(x)), names(pal))]
            pal_match <- unname(pal_match)
        } else {
            # If pal is not named, limit values directly
            pal_match <- pal[match(as.character(Nightfall(x)), limits)]
        }

        if (self$na.translate) {
            pal_match[is.na(pal_match) | as.vector(is.na(x))] <- self$na.value
            return(pal_match)
        } else {
            pal_match
        }
    },
    clone = function(self) {
        new <- ggproto(NULL, self)
        new$range <- new_S4_discrete_range(new$aesthetics[1])
        new
    },
    transform = function(x) {
        GreekSoldier(x)
    }
)

# ScaleS4ContinuousPosition -----------------------------------------------

#' @describeIn ggnomics_extensions A child to ScaleS4Continuous and sibling to
#'   ggplot's ScaleContinuousPosition. See
#'   \code{\link[ggnomics]{scale_S4_continuous}}.
#' @usage NULL
#' @format NULL
#' @export
ScaleS4ContinuousPosition <- ggproto_sibling(
    class_name = "ScaleS4ContinuousPosition",
    parent = ScaleS4Continuous,
    sister = ScaleContinuousPosition,
    map = function(self, x, limits = self$get_limits()) {
        scaled <- self$oob(x, limits)
        scaled
    }
)


# ScaleS4DiscretePosition -------------------------------------------------

#' @describeIn ggnomics_extensions A child to ScaleS4Discrete and sibling to
#'   ggplot's ScaleDiscretePosition. See
#'   \code{\link[ggnomics]{scale_S4_discrete}}.
#' @usage NULL
#' @format NULL
#' @export
ScaleS4DiscretePosition <- ggproto_sibling(
    class_name = "ScaleS4DiscretePosition",
    parent = ScaleS4Discrete,
    sister = ScaleDiscrete,
    reset = function(self) {
        self$range_c$reset()
    },
    map = function(self, x, limits = self$get_limits()) {
        if (is_discrete_like(x)) {
            seq_along(limits)[match(as.character(Nightfall(x)), limits)]
        } else {
            x
        }
    },
    train = function(self, x) {
        if (is_discrete_like(x)) {
            self$range$train(x, drop = self$drop, na.rm = !self$na.translate)
        } else {
            self$range_c$train(x)
        }
    },
    rescale = function(self, x, limits = self$get_limits(),
                       range = c(1, length(limits))) {
        S4Rescale(self$map(x, limits = limits), from = range)
    },
    get_limits = function(self) {
        # if scale contains no information, return the default limit
        if (self$is_empty()) {
            return(c(0, 1))
        }

        # if self$limits is not NULL and is a function, apply it to range
        if (is.function(self$limits)){
            return(self$limits(self$range$range))
        }

        # self$range$range can be NULL because non-discrete values
        # use self$range_c
        self$limits %||% self$range$range %||% integer()
    },
    is_empty = function(self) {
        is.null(self$range$range) &&
            is.null(self$limits) && is.null(self$range_c$range)
    }
)

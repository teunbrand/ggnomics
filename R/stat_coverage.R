# Constructor -------------------------------------------------------------

#' @name stat_coverage
#' @title Coverage
#'
#' @description Calculates how often a space is covered by a set of ranges.
#'
#' @inheritParams ggplot2::stat_density
#' @param orientation The orientation of the layer. The default (\code{NA})
#'   automatically determines the orientation from the aesthetics mapping. In
#'   the rare event that this fails, it can be given explicitly by setting
#'   \code{orientation} to either \code{"x"} or \code{"y"}. See the
#'   \emph{Orientation} section of
#'   \code{\link[ggplot2]{geom_density}()}.
#'
#' @details For \linkS4class{IRanges} and \linkS4class{GRanges} classes, makes
#' use of the \code{\link[IRanges]{coverage}} function. When data is
#' \code{numeric}, be sure to also set the \code{xend} or \code{yend} aesthetic.
#'
#' @note The \code{Ranges} classes as \code{x|y} are considered closed-interval,
#' whereas \code{x|y} and \code{xend|yend} with numeric data are considered open
#' interval.
#'
#' @section Aesthetics: \code{stat_coverage} understands the following
#' aesthetics (required aesthetics are in bold, optional in italic).
#' \itemize{
#'  \item \strong{x}
#'  \item \strong{y}
#'  \item \emph{xend} (if data is numeric)
#'  \item \emph{yend} (if data is numeric)
#'  \item alpha
#'  \item colour
#'  \item fill
#'  \item group
#'  \item linetype
#'  \item size
#'  \item weight
#' }
#'
#' @section Computed variables:
#' \describe{
#'   \item{coverage}{coverage}
#'   \item{scaled}{coverage scaled to a maximum of 1}
#'   \item{density}{coverage integrated to 1}
#' }
#'
#' @export
#' @examples
#' # Computing coverage on numeric data
#' df <- data.frame(min = c(1, 6, 11),
#'                  max = c(9, 13, 15))
#' ggplot(df, aes(x = min, xend = max)) +
#'   stat_coverage()
#'
#' # Computing coverage on Ranges data
#' require(GenomicRanges)
#'
#' df <- DataFrame(x = GRanges(c("chr1:100-200", "chr1:140-260",
#'                               "chr2:50-100")))
#' ggplot(df, aes(x)) +
#'   stat_coverage()
stat_coverage <- function(
    mapping = NULL, data = NULL,
    geom = "area", position = "identity",
    ..., trim = FALSE, na.rm = FALSE, orientation = NA, show.legend = NA,
    inherit.aes = TRUE) {
    layer(
        data = data,
        mapping = mapping,
        stat = StatCoverage,
        geom = geom,
        position = position,
        show.legend = show.legend,
        inherit.aes = inherit.aes,
        params = list(
            trim = trim,
            na.rm = na.rm,
            orientation = orientation,
            ...
        )
    )
}

# ggproto -----------------------------------------------------------------

StatCoverage <- ggproto(
    "StatCoverage",
    Stat,
    required_aes = "x|y",
    default_aes = aes(x = after_stat(coverage),
                      y = after_stat(coverage),
                      fill = NA, weight = 1L),
    optional_aes = "xend|yend",
    aesthetics = function(self) {
        if (is.null(self$required_aes)) {
            required_aes <- NULL
        } else {
            required_aes <- .flatstrsplit(self$required_aes, "|")
        }
        if (is.null(self$optional_aes)) {
            optional_aes <- NULL
        } else {
            optional_aes <- .flatstrsplit(self$optional_aes, "|")
        }

        c(union(required_aes, names(self$default_aes)), optional_aes, "group")
    },
    setup_params = function(data, params) {
        params$flipped_aes <- .int$has_flipped_aes(
            data, params,
            main_is_orthogonal = FALSE,
            main_is_continuous = TRUE
        )

        has_x <- !(is.null(data$x) && is.null(params$x))
        has_y <- !(is.null(data$y) && is.null(params$y))
        if (!has_x && !has_y) {
            rlang::abort("stat_coverage() requires an x or y aesthetic.")
        }

        if (has_x && inherits(Nightfall(data$x), "Ranges")) {
            params$cov_range <- TRUE
        } else if (has_y && inherits(Nightfall(data$y), "Ranges")) {
            params$cov_range <- TRUE
        } else {
            params$cov_range <- FALSE

            has_xend <- !(is.null(data$xend) && is.null(params$xend))
            has_yend <- !(is.null(data$yend) && is.null(params$yend))
            if (!has_xend && !has_yend) {
                msg <- paste("stat_coverage() requires xend or yend",
                             "aesthetic for numeric data.")
                rlang::abort(msg)
            }
        }
        params
    },
    extra_params = c("na.rm", "orientation"),
    compute_group = function(data, scales, flipped_aes = FALSE, trim = FALSE,
                             cov_range = FALSE) {
        if (is.null(data) || nrow(data) == 0) {
            stop("Cannot compute coverage with no data points.")
        }
        flipper <- ggplot2:::flipped_names(flipped_aes)$x
        if (trim) {
            range <- suppressWarnings(range(data$x, na.rm = TRUE))
        } else {
            range <- scales[[flipper]]$dimension()
        }

        if (cov_range) {
            coverage <- compute_coverage_Ranges(data$x, data$weight,
                                                range = range)
        } else {
            valid_x <- is.numeric(data$x) && is.numeric(data$xend)
            if (valid_x) {
                coverage <- compute_coverage_numeric(data$x, data$xend,
                                                     data$weight,
                                                     range = range)
            } else {
                stop("Cannot compute coverage.")
            }
        }
    },
    compute_layer = function(self, data, params, layout) {
        data <- ggplot2:::flip_data(data, flip <- params$flipped_aes)
        data <- ggproto_parent(Stat, self)$compute_layer(data = data,
                                                         params = params,
                                                         layout = layout)
        data$flipped_aes <- flip
        ggplot2:::flip_data(data, flip)
    }
)

# Ranges coverage ---------------------------------------------------------

compute_coverage_Ranges <- function(x, weight = NULL, range = S4Range(x)) {
    x <- Nightfall(x)
    if (is.null(weight)) {
        weight <- 1L
    } else {
        weight <- Nightfall(weight)
    }
    range <- Nightfall(range)

    if (!hasMethod("coverage", class(x)[[1]])) {
        rlang::abort("Class ", class(x)[[1]], "has no coverage method.")
    }

    dat <- coverage(x, weight = weight)
    if (inherits(x, "GenomicRanges")) {
        dat <- lapply(names(dat), function(i) {
            rle <- dat[[i]]
            GPos(factor(i, levels = names(dat)),
                 c(start(rle), end(rle), tail(end(rle), 1) + 1),
                 score = c(runValue(rle), runValue(rle), 0))
        })
        dat <- sort(do.call(c, dat))
        dat <- dat[overlapsAny(dat, range + 1)]
    } else if (inherits(x, "IntegerRanges")) {
        dat <- IPos(
            c(start(dat), end(dat), tail(end(dat), 1) + 1),
            score = c(runValue(dat), runValue(dat), 0)
        )
        dat <- sort(dat)
        dat <- dat[pos(dat) >= min(range - 1) & pos(dat) <= max(range + 1)]
    } else {
        stop("Don't know how to compute coverage for class ",
             class(x)[[1]], ".")
    }
    score <- mcols(dat)$score
    dat <- list(x = GreekSoldier(dat[, -1]),
                coverage = score,
                scaled = score / max(score),
                density = score / sum(width(x) * weight))
    class(dat) <- "data.frame"
    attr(dat, "row.names") <- .set_row_names(length(score))
    dat
}

# Numeric coverage --------------------------------------------------------

# Note that numeric doesn't have discrete positions and don't follow the closed
# interval logic that the BioC Ranges have. I.e. if we have 1-2 and 2-3 as
# ranges, the coverage will be 1-1-1 for the three positions.
# In contrast, coverage(IRanges(1:2, 2:3)) will have coverage 1-2-1

compute_coverage_numeric <- function(min, max, weight = NULL,
                                     range = range(c(min, max)))
{
    if (is.null(weight)) {
        weight <- rep_len(1L, length.out = length(min))
    } else if (length(weight) == 1) {
        weight <- rep_len(weight, length.out = length(min))
    }

    # Order all relevant positions
    pos <- c(pmin(min, max), pmax(min, max))
    o <- order(pos)
    pos <- pos[o]

    # Order weights; end-positions have negative weight
    w <- c(weight, -weight)[o]
    # Sum identical positions
    w <- vapply(split(w, pos), sum, numeric(1))
    # Cumulative sum over + and - weights yields coverage
    w <- cumsum(w)

    # To account for having vapplied over 'w'
    pos <- unique(pos)

    pos <- rep(pos, each = 2)
    w <- c(0, head(rep(w, each = 2), -1))

    structure(list(
        x = pos, coverage = w, scaled = w / max(w),
        density = w / sum(abs(min - max) * weight)
    ), class = "data.frame", row.names = .set_row_names(length(pos)))
}

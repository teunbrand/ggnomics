#' @include scales_class_S4.R
NULL

# ScaleGenomic is a ScaleS4ContinuousPosition scale with a different default
# number of minor breaks and different default axis guide.

#' @export
#' @usage NULL
#' @describeIn ggnomics_extensions A child to ScaleS4ContinuousPosition for
#'   genomic coordinates. See \code{\link[ggnomics]{scale_genomic}}.
#' @format NULL
ScaleGenomic <- ggproto(
    "ScaleGenomic",
    ScaleS4ContinuousPosition,
    get_breaks_minor = function(self, n = 5,
                                b = self$break_positions(),
                                limits = self$get_limits()) {
        ggproto_parent(ScaleS4Continuous, self)$get_breaks_minor(
            n = n, b= b, limits = limits
        )
    },
    get_labels = function(self, breaks = self$get_breaks()) {

        if (is.null(breaks)) {
            return(NULL)
        }

        breaks <- self$trans$inverse(breaks)

        if (is.null(self$labels)) {
            return(NULL)
        }

        if (identical(self$labels, NA)) {
            stop("Invalid labels specification. Use NULL, not NA")
        }

        if (inherits(self$labels, "waiver")) {
            labels <- self$trans$format(breaks)
        } else if (is.function(self$labels)) {
            labels <- self$labels(breaks)
        } else {
            labels <- self$labels
        }

        if (length(labels) != length(breaks)) {
            if (inherits(Nightfall(breaks), "ANYGenomic")) {
                # Attempt to rescue labels by pasting them as seqnames
                sqlvl <- GenomeInfoDb::seqlevels(Nightfall(breaks))
                if (length(labels) == length(sqlvl)) {
                    sqnm <- seqnames(Nightfall(breaks))
                    levels(runValue(sqnm)) <- labels
                    labels <- as.character(decode(sqnm))
                } else {
                    stop("Breaks and labels are different lengths")
                }
            } else {
                stop("Breaks and labels are different lengths")
            }
        }

        labels
    },
    guide = "genomic_axis"
)

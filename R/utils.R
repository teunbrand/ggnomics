# This probably doesn't belong here, but as long as S4 classes don't have format
# methods I'm keeping it.

#' @export
#' @noRd
#' @method format Vector
format.Vector <- function(x, ...) {
    showAsCell(x)
}

# Check finite ------------------------------------------------------------

# Should check wether data is finite like base::is.finite
setGeneric(
    "check_finite",
    function(x) standardGeneric("check_finite")

)

setMethod(
    "check_finite",
    signature = c(x = "ANY"),
    definition = function(x) is.finite(x)
)

# Since
setMethod(
    "check_finite",
    signature = c(x = "Vector"),
    definition = function(x) {
        fun <- selectMethod("is.finite", class(x))
        if (is.primitive(fun) || is.null(fun)) {
            valid <- validObject(x)
            return(rep(valid, length(x)))
        } else {
            return(is.finite(x))
        }
    }
)


# Import ggplot internals -------------------------------------------------

.grab_ggplot_internals <- function() {
    objects <- c(
        "check_required_aesthetics", # in position disjoint ranges
        "check_breaks_labels",    # in scale constructors
        "default_expansion",      # in viewscales
        # "expand_limits_discrete", # in viewscales
        "is_position_aes",        # in scale constructors
        "make_labels",            # in ggplot constructor
        "scales_list",            # in ggplot constructor
        "set_sec_axis",           # in scale constructor
        # "view_scale_secondary",   # in viewscales
        "scale_flip_position",    # in viewscales
        "new_data_frame",         # in guide_axis_genomic
        "axis_label_element_overrides", # in guide_axis genomic
        "trans_support_nbreaks",  # in scales class S4
        "draw_axis_labels",       # in guide_axis genomic
        "warn_for_guide_position",# in guide_axis genomic
        "check_transformation",   # in S4 scales
        "snake_class",            # in position disjoint ranges
        "has_flipped_aes" # in stat coverage
    )
    objects <- setNames(objects, objects)
    out <- lapply(objects, function(i) {
        getFromNamespace(i, "ggplot2")
    })
}

.int <- .grab_ggplot_internals()

# Global variables --------------------------------------------------------

# Since ggproto works similar to R6 reference classes, we'd best define "self"
# as a global variable for check
globalVariables(
    "self"
)

.glob <- rlang::new_environment(
    list(
        x_aes = c("x", "xmin", "xmax", "xend", "xintercept", "xmin_final",
                  "xmax_final", "xlower", "xmiddle", "xupper", "x0"),
        y_aes = c("y", "ymin", "ymax", "yend", "yintercept", "ymin_final",
                  "ymax_final", "lower", "middle", "upper", "y0")
    )
)

# Miscellaneous -----------------------------------------------------------

is_discrete_like <- function(x) {
    if (inherits(x, "Rle")) {
        x <- runValue(x)
    }
    if (inherits(x, "Factor")) {
        x <- levels(x)
    }
    is.factor(x) || is.character(x) || is.logical(x) || is(x, "knownDiscretes")
}

## Here be ggplot methods that require special behaviour from the plot
## constructor.
## For now, we'd just need to add the coord_S4 as the default whenever the input
## inherits from Vector.
## Might need more specialised behaviour for the more complicated data
## containers i.e. SummarizedExperiment and derivatives when it can't be handled
## at the fortify stage.

#' @export
#' @method ggplot Vector
#' @noRd
ggplot.Vector <- function(data = NULL,
                          mapping = aes(),
                          ...,
                          environment = parent.frame()) {
    if (!missing(mapping) && !inherits(mapping, "uneval")) {
        rlang::abort("Mapping should be created with `aes()` or `aes_()`.")
    }

    data <- fortify(data, ...)

    p <- structure(list(
        data = data,
        layers = list(),
        scales = .int$scales_list(),
        mapping = mapping,
        theme = list(),
        coordinates = coord_S4(default = TRUE),
        facet = facet_null(),
        plot_env = environment
    ), class = c("gg", "ggplot"))

    p$labels <- .int$make_labels(mapping)

    set_last_plot(p)
    p
}

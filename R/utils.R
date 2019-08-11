
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun , "`.\n",
       "Please install and try again.", call. = FALSE)
}

`%||%` <- function(x, y) {
  if (is.null(x))
    y
  else x
}

.grab_ggplot_internals <- function() {
  objects <- c(
    ".all_aesthetics", "as_facets_list", "as_gg_data_frame", "check_aesthetics",
    "check_labeller", "compact", "convertInd", "df.grid", "empty", "eval_facets", "ggname",
    "grid_as_facets_list", "is.zero", "sanitise_dim", "snake_class", "ulevels",
    "unique_combs", "var_list", "weave_tables_col", "weave_tables_row", ".pt"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

.int <- .grab_ggplot_internals()

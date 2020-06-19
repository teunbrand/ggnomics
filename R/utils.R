
try_require <- function(package, fun) {
  if (requireNamespace(package, quietly = TRUE)) {
    return(invisible())
  }

  stop("Package `", package, "` required for `", fun, "`.\n",
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
    "check_labeller", "check_subclass", "compact", "convertInd", "df.grid", 
    "defaults",
    "empty", "eval_facets", "ggname", "rename_aes", "mapped_aesthetics",
    "make_labels",
    "grid_as_facets_list", "is.zero", "sanitise_dim", "set_draw_key",
    "snake_class", "ulevels",
    "unique_combs", "var_list", "validate_mapping", 
    "weave_tables_col", "weave_tables_row", ".pt"
  )
  objects <- setNames(objects, objects)
  out <- lapply(objects, function(i) {
    getFromNamespace(i, "ggplot2")
  })
}

.int <- .grab_ggplot_internals()

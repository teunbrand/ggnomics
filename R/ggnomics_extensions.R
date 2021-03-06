#' @name ggnomics_extensions
#'
#' @title ggnomics extensions to ggplot2
#'
#' @description ggnomics relies on the extension mechanism of ggplot2 through
#'   ggproto class objects, which allows cross-package inheritance of objects
#'   such as geoms, stats, facets, scales and coordinate systems. These objects
#'   can be ignored by users for the purpose of making plots, since interacting
#'   with these objects is preferred through various geom_\*, stat_\*, facet_\*,
#'   coord_\* and scale_\* functions.
#'   
#'   Note that the below says \strong{Functions}, but actually describes ggproto
#'   classes.
#'
#' @seealso \link[ggplot2]{ggplot2-ggproto}
NULL

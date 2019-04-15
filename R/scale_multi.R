#' Multiple gradient colour scales
#'
#' @description Maps multiple aesthetics to multiple colour fill gradient
#'   scales. It takes in listed arguments for each aesthetic and disseminates
#'   these to \code{\link[ggplot2]{continuous_scale}}.
#'
#' @param ...,colours,values,space,na.value,guide,colors listed arguments in
#'   \code{\link[ggplot2]{scale_colour_gradientn}} (e.g. \code{colours =
#'   list(c("white", "red"), c("black", "blue"))}).
#' @param aesthetics a \code{character} vector with names of aesthetic mapping.
#'
#' @details This function should only be called after all layers that this
#'   function affects are added to the plot.
#'
#'   The list elements of the listed arguments are assumed to follow the
#'   \code{aesthetics} order, i.e. the n\emph{th} list element belongs to the n\emph{th}
#'   aesthetic. When there are more list elements than n aesthetics, only the
#'   first n\emph{th} list elements are taken. When there are more \code{aesthetics}
#'   than list elements, the first list element is used for the remaining
#'   aesthethics.
#'
#'   In contrast to other \code{scale_*_continous}-family functions, the
#'   \code{guide} argument is interpreted before adding it to the plot instead
#'   of at the time of plot building. This behaviour ensures that the
#'   \code{available_aes} argument of the guides are set correctly, but may
#'   interfere with the \code{\link[ggplot2]{guides}} function.
#'
#' @return A nested list-like structure of the class \code{MultiScale}.
#'
#' @export
#'
#' @examples
#' # Setup dummy data
#' df <- rbind(data.frame(x = 1:3, y = 1, v = NA, w = 1:3, z = NA),
#'             data.frame(x = 1:3, y = 2, v = 1:3, w = NA, z = NA),
#'             data.frame(x = 1:3, y = 3, v = NA, w = NA, z = 1:3))
#'
#' ggplot(df, aes(x, y)) +
#'   geom_raster(aes(fill1 = v)) +
#'   geom_raster(aes(fill2 = w)) +
#'   geom_raster(aes(fill3 = z))
#'   scale_fill_multi(aesthetics = c("fill1", "fill2", "fill3"),
#'                    colours = list(c("white", "red"),
#'                                   c("black", "blue"),
#'                                   c("grey50", "green")))
scale_fill_multi <- function(..., colours, values = NULL, space = "Lab", na.value = "transparent",
                             guide = "colourbar", aesthetics = "fill", colors){

  # Convert to proper spelling of colour
  colours <- if (missing(colours))
    if (missing(colors))
      c("white", "black")
  else colors
  else colours

  # Digest extra arguments
  extra.args <- lapply(seq(aesthetics), function(i){
    lapply(list(...), pickvalue, i)
  })

  # Choose guide
  guide <- lapply(seq(aesthetics), function(i){
    this.guide <- pickvalue(guide, i)
    if (all(class(this.guide) == "character") && length(this.guide) == 1) {
      if(standardise_aes_names(this.guide) == standardise_aes_names("colourbar")){
        this.guide <- guide_colourbar()
      } else if (this.guide == "legend") {
        this.guide <- guide_legend()
      }
    }
    if (any(class(this.guide) == "guide")) {
      this.guide$available_aes <- aesthetics[[i]]
    } else {
      stop("I haven't programmed this path yet.\nChoose a legend or colourbar guide", call. = FALSE)
    }
    return(this.guide)
  })

  # Build scales
  scales <- lapply(seq(aesthetics), function(i){
    aes <- aesthetics[[i]]
    pass.args = list(
      aesthetics = aes,
      scale_name = paste0("MultiScale_", aes),
      palette = scales::gradient_n_pal(colours = pickvalue(colours, i),
                                       values  = pickvalue(values, i),
                                       space   = pickvalue(space, i)),
      na.value = pickvalue(na.value, i),
      guide = pickvalue(guide, i)
    )
    pass.args <- append(pass.args, pickvalue(extra.args, i))
    out <- do.call("continuous_scale", pass.args)
    return(out)
  })

  structure(list(scales = scales, aes = aesthetics), class = "MultiScale")
}

pickvalue <- function(x, i){
  if (class(x)[[1]] != "list"){
    return(x)
  } else {
    len <- length(x)
    i <- if (i > length(x))
      1
    else i
    return(x[[i]])
  }
}

#' @export
ggplot_add.MultiScale <- function(object, plot, object_name){
  for(i in object$scales){
    plot$scales$add(i)
  }

  plot$layers <- lapply(plot$layers, function(lay){
    if(!(names(lay$mapping) %in% object$aes)) {
      return(lay)
    }
    new.aes <- object$aes[object$aes %in% names(lay$mapping)]
    old.geom <- lay$geom
    old.geom.nahandle <- old.geom$handle_na
    new.geom.nahandle <- function(self, data, params){
      colnames(data)  <- gsub(new.aes, "fill", colnames(data))
      old.geom.nahandle(data, params)
    }

    new.geom <-
      ggproto(paste0("New", new.aes, class(old.geom)[1]),
              old.geom,
              handle_na =
                new.geom.nahandle,
              default_aes =
                setNames(old.geom$default_aes,
                         gsub("^fill$", new.aes, names(old.geom$default_aes))),
              non_missing_aes =
                gsub("^fill$", new.aes, old.geom$non_missing_aes),
              optional_aes =
                gsub("^fill$", new.aes, old.geom$optional_aes),
              required_aes =
                gsub("^fill$", new.aes, old.geom$required_aes))
    lay$geom <- new.geom
    return(lay)
  })
  return(plot)
}

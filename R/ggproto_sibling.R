#' Like 'ggproto', but also captures methods from 'sister' in addition to the
#' usual parental inheritance. Used in the ScaleS4-family of scale ggprotos.
ggproto_sibling <- function(
  class_name = character(),
  parent = NULL,
  sister = NULL,
  ...
) {
  e <- new.env(parent = emptyenv())

  myself <- list(...)
  if (length(myself) != sum(nzchar(myname <- names(myself)))) {
    stop("Members must have names")
  }

  if (length(myself) > 0) {
    list2env(myself, envir = e)
  }

  parent <- substitute(parent)
  sister <- substitute(sister)
  env <- parent.frame()

  find_sister <- function() eval(sister, env, NULL)
  find_parent <- function() eval(parent, env, NULL)

  dory <- find_sister()
  nemo <- find_parent()

  if (!is.null(nemo)) {
    if (!is.ggproto(nemo)) {
      stop("Must provide valid parent")
    }
    e$super <- find_parent
  }

  iforgot   <- names(dory)
  iremember <- union(names(e), names(nemo))
  for (i in iforgot) {
    if (i %in% iremember || isTRUE(i == "call")) {
      next()
    }
    e[[i]] <- copy_method(i, dory)
  }

  if (!is.null(nemo)) {
    class(e) <- c(class_name, class(nemo))
  } else {
    class(e) <- c(class_name, "ggproto", "gg")
  }

  e
}

#' Copy inner function from ggproto methods
copy_method <- function(what, from) {
  this <- from[[what]]
  if (is.null(this)) {
    return(NULL)
  }
  if (is(this)[[1]] == "ggproto_method") {
    this <- as.list(environment(this))$f
  }
  return(this)
}

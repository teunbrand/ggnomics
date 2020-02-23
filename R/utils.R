#' @export
#' @noRd
#' @method format Vector
format.Vector <- function(x, ...) {
  showAsCell(x)
}
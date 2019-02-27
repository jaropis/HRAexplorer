#' alternative for null
#'
#' @param x value being checked for null
#' @param y default expression value
#' @return x if not null, y otherwise
#'
#' @export
`%||%` <- function(x, y) {
  if (is.null(x))
    return(y)
  x
}

#' Test existance of a variable
#'
#' A very simple utility function that tests whether anything in the Global
#' environment has been defined by the given name
#'
#' @param name the name to search for
#'
#' @examples
#' x <- 42
#' defined("x") #TRUE
#' defined("y") #FALSE
#' @export
defined <- function(name) {
    name %in% objects()
}

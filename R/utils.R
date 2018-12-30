#' Test existance, class and value of a variable
#'
#' A utility function that tests whether an object has been defined by the given name
#' with the given value, and with specified possible classes in the specified environment.
#'
#' @param name the name of the object to search for
#' @param value the value of object
#' @param obj_classes the allowed class(es) of the object. If \code{NULL}, no class check is performed
#' @param envir the environment to search in; defaults to global
#' @examples
#' x <- 42
#' is_defined_with_value("x", 42) #TRUE
#' is_defined_with_value("x", 42, c("numeric", "integer")) #TRUE
#'
#' @export
is_defined_with_value <- function(name, value, obj_classes=NULL, envir=.GlobalEnv) {
    if(! is_defined(name, envir)) return (FALSE)
    x <- get(name, envir = envir)
    #print(x)
    if((! is.null(obj_classes)) && (! class(x) %in% obj_classes) ) {return(FALSE)}
    return(x == value)
}

#' Test existance of a variable
#'
#' A utility function that tests whether an object has been defined by the given name
#'  in the specified environment.
#'
#' @param name the name of the object to search for
#' @param envir the environment to search in; defaults to global
#' @examples
#' x <- 42
#' is_defined("x") #TRUE
#'
#' @export
is_defined <- function(name, envir=.GlobalEnv) {
    return(name %in% objects(envir=envir))
}

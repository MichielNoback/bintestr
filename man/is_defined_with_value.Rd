% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{is_defined_with_value}
\alias{is_defined_with_value}
\title{Test existance, class and value of a variable}
\usage{
is_defined_with_value(name, value, obj_classes = NULL,
  envir = .GlobalEnv)
}
\arguments{
\item{name}{the name of the object to search for}

\item{value}{the value of object}

\item{obj_classes}{the allowed class(es) of the object. If \code{NULL}, no class check is performed}

\item{envir}{the environment to search in; defaults to global}
}
\description{
A utility function that tests whether an object has been defined by the given name
with the given value, and with specified possible classes in the specified environment.
}
\examples{
x <- 42
is_defined_with_value("x", 42) #TRUE
is_defined_with_value("x", 42, c("numeric", "integer")) #TRUE

}

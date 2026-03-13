\name{Concatenate}
\docType{methods}
\alias{Concatenate}
\alias{c}
\alias{c,frab-method}
\title{Concatenation methods for class \code{"frab"}}
\description{
  Concatenation does not make sense for \code{frab} objects and will
  return an error.  For combining objects, use \code{a+b}.
}
\usage{\S4method{c}{frab}(x, \dots)}
\arguments{\item{x, \dots}{Objects to be concatenated}}
\value{No return value, throws an error.}
\section{Methods}{
  \describe{
    \item{\code{signature(x = "frab")}}{Method for \code{c()} to prevent
      concatenation.}}
}

\name{misc}
\alias{misc}
\alias{length}
\alias{length.frab}
\alias{length,frab-method}
\alias{lapply}
\alias{lapply,disord-method}
\alias{lapply.disord}
\alias{is.na}
\alias{is.na.frab}
\alias{is.na<-}
\alias{is.na<-.frab}
\alias{is.na,frab-method}
\alias{match}
\alias{match,frab-method}
\alias{match,frab,ANY-method}
\alias{match,ANY,frab-method}
\alias{match,ANY,frab-method}
\alias{match,frab,frab-method}
\alias{\%in\%}
\alias{\%in\%,frab-method}
\alias{\%in\%,frab,ANY-method}
\alias{\%in\%,ANY,frab-method}
\alias{\%in\%,ANY,frab-method}
\alias{\%in\%,frab,frab-method}
\alias{!,frab-method}
\alias{which,frab-method}
\alias{which,disindex-method}
\title{Miscellaneous functions}
\usage{
\S4method{length}{frab}(x)
}
\arguments{\item{x}{Object of class \code{frab}}}
\description{

This page documents various functions that work for frabs, and I will
add to these from time to time as I add new functions that make sense
for frab objects.  Functions like \code{sin()} and \code{abs()} should
work as expected: they take and return \code{frab} objects with the same
hash (which means that idiom like \code{x + sin(x)} is accepted).
However, there are a few functions that are a little more involved:

\itemize{
  \item{length()} returns the length of the data component of the
  object.
  \item\code{sapply(X,f)} returns a frab object which is the result of
  applying \code{f()} to each element of \code{X}.
  \item\code{which()} returns a \code{disind} object when given a
  Boolean  \code{frab}
}
}
\value{Generally return frabs}
\author{Robin K. S. Hankin}
\note{
note here
}
\seealso{\code{\link{extract}}}
\examples{
2+2
}

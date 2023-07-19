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
\alias{is.na<-.frab}
\alias{is.na,frab-method}
\alias{is.na<-.frab}
\alias{is.na<-,frab,disord-method}
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
  Boolean \code{frab}
}
}
\value{Generally return frabs}
\author{Robin K. S. Hankin}
\note{
note here
}
\seealso{\code{\link{extract}}}
\examples{


(a <- frab(c(a=1,b=NA,c=44,x=NA,h=4)))
is.na(a)

(x <- frab(c(x=5,y=2,z=3,a=7,b=6)))
which(x>3)
x[which(x>3)]
x[which(x>3)] <- 4
x

is.na(x) <- x<3
x
x[is.na(x)] <- 100
x
}

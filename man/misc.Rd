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
\alias{is.na,frab-method}
\alias{is.notna}
\alias{is.notna.frab}
\alias{is.notna,frab-method}
\alias{is.na<-}
\alias{is.na<-.frab}
\alias{is.na<-.frab}
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
for frab objects.  To use functions like \code{sin()} and \code{abs()}
on \code{frab} object \code{x}, work with \code{values(x)} (which is a
\code{disord} object).  However, there are a few functions that are a
little more involved:

\itemize{
  \item\code{length()} returns the length of the data component of the
  object.
  \item\code{which()} returns an error when called with a \code{frab}
  object, but is useful here because it returns a \code{disind} when
  given a Boolean \code{disord} object.  This is useful for idiom such
  as \code{x[x>0]}
  \item Functions \code{is.na()} and \code{is.notna()} return a
  \code{disind} object}
}
\value{Generally return frabs}
\author{Robin K. S. Hankin}
\note{
  Constructions such as \code{!is.na(x)} do not work if \code{x} is a
  \code{frab} object: this is because \code{is.na()} returns a
  \code{disind} object, not a logical.  Use \code{is.notna()} to
  identify elements that are not \code{NA}.
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

y <- frab(c(a=5,b=NA,c=3,d=NA))
y[is.notna(y)] <- 199
y
}

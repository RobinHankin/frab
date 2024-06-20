\name{Summary-methods}
\docType{methods}
\alias{Summary-methods}
\alias{Summary,frab-method}
\title{Methods for Function \code{Summary}}
\description{

  Methods for \proglang{S4} function \code{Summary} in the \pkg{frab}
  package.  Currently, only \code{max()}, \code{min()}, \code{range()}
  and \code{sum()} are defined, and these operate in the natural way on
  the elements of a \code{frab}.  Note that these functions are not
  susceptible to \CRANpkg{disordR} violations.
}
\section{Methods}{
\describe{\item{\code{signature(x = "frab")}}{Dispatches to
\code{max(values(x))} etc.}}
}
\keyword{methods}
\examples{
a <- rfrab()
a
max(a)
min(a)
range(a)
}

\name{frab-class}
\docType{class}
\alias{frab-class}
\title{Class \dQuote{frab}}
\description{The formal \proglang{S4} class for frab objects}
\section{Objects from the Class}{
  Formal class \dfn{frab} has a single slot \code{x} which is a named
  numeric vector.  The class has arithmetic, comparison, summary, and
  mathematical methods defined for it.
}
\author{Robin K. S. Hankin}
\examples{

new("frab",x=c(a=6,b=4,c=1))   # formal creation method (discouraged)


frab(c(a=4,b=1,c=5))   # use frab() in day-to-day work

frab(c(a=4,b=0,c=5))   # zero entries are discarded

frab(c(a=4,b=3,b=5))   # repeted entries are summed

frab(c(apple=4,orange=3,cherry=5))   # any names are OK

x <- frab(c(d=1,y=3,a=2,b=5,rug=7,c=2))
(y <- rfrab())

x+y          # addition works as expected
x + 2*y      # arithmetic
x>2          # extraction
x[x>3] <- 99 # replacement


# sum(x)       # some summary methods implemented
# max(x) 

}
\keyword{classes}
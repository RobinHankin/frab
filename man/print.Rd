\name{print}
\alias{print}
\alias{print.frab}
\alias{frab_print}
\alias{show,frab-method}
\title{Methods for printing frabs}
\description{Methods for printing frabs nicely}
\usage{
\S4method{show}{frab}(object)
frab_print(object)
}
\arguments{
  \item{object}{An object of class \code{frab}}
}
\details{
  The method is sensitive to option \code{frab_print_hash}.  If
  \code{TRUE}, the hash code is printed; otherwise it is not.

  Function \code{frab_print()} returns its argument, invisibly.

  There is special dispensation for the empty \code{frab} object.
  
}
\value{Returns its argument, invisibly}
\author{Robin K. S. Hankin}
\examples{

print(rfrab())  # default

options(frab_print_hash = TRUE)
print(rfrab())  # prints hash code

options(frab_print_hash = NULL)  # restore default

}
\keyword{math}

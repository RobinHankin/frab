\name{namedvector}
\alias{is.namedvector}
\alias{as.namedvector}
\alias{is.namedlogical}
\alias{is.unnamedlogical}
\alias{is.unnamedvector}
\title{Named vectors and the frab package}
\description{
Named vectors are closely related to \code{frab} objects, but are not
the same.  However, there is a natural coercion from one to the other.
}
\usage{
is.namedvector(v)
is.namedlogical(v)
is.unnamedlogical(v)
is.unnamedvector(v)
}
\arguments{
  \item{v}{Argument to be tested or coerced}
}
\details{
  Coercion and testing for named vectors.
  Function \code{nv_to_frab()}, documented at \code{frab.Rd}, coerces a
  named vector to a \code{frab}.

}
\value{
Function \code{is.namedvector()} returns a boolean, function
\code{as.namedvector()} returns a named vector.
}
\author{Robin K. S. Hankin}
\examples{
x <- c(a=5, b=3, c=-2,b=-3, x=33)
is.namedvector(x)
as.namedvector(frab(x))



x <- c(a=5, b=3, c=-2)
y <- c(p=1, c=2, d= 6)

x
y
x+y

frab(x) + frab(y)

}

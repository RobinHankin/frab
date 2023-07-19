\name{zero}
\alias{zero}
\alias{is.zero}
\alias{empty}
\alias{is.empty}
\title{The zero frab object}
\description{\loadmathjax
  Test for a \code{frab} object's being zero (empty).
}
\usage{
zero(...)
is.zero(x)
is.empty(x)
}
\arguments{
  \item{x}{Object of class \code{frab}}
  \item{...}{Further arguments (currently ignored)}
}
\details{

  Function \code{zero()} returns the empty \code{frab} object; this is
  the additive identity \mjseqn{0} with property \mjseqn{x+0=0+x=x}.

  Function \code{is.zero()} returns \code{TRUE} if its argument is
  indeed the zero object.

  Function \code{is.empty()} is a synonym for \code{is.zero()}.
  Sometimes one is thinking about the free Abelian group, in which case
  \code{is.zero()} makes more sense, and sometimes one is thinking about
  maps and tables, in which case \code{is.empty()} is more appropriate.
}
\value{Function \code{zero()} returns the zero frab object, function
  \code{is.zero()} a Boolean}
\author{Robin K. S. Hankin}
\examples{

zero()
zero() + zero()

x <- rfrab()

x+zero() == x

is.zero(zero())
}

\keyword{symbolmath}

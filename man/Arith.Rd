\name{Arith}
\alias{arith}
\alias{Arith,frab,missing-method}
\alias{Arith,frab,frab-method}
\alias{Arith,frab,frab-method}
\alias{Arith,frab,numeric-method}
\alias{Arith,ANY,frab-method}
\alias{Arith,frab,ANY-method}
\alias{frab_multiply_numeric}
\alias{frab_power_numeric}
\alias{numeric_multiply_frab}
\alias{numeric_power_frab}
\alias{frab_negative}
\alias{frab_reciprocal}
\alias{frab_plus_frab}
\alias{frab_plus_numeric}
\alias{frab_multiply_frab}
\alias{frab_unary}
\alias{frab_arith_frab}
\alias{frab_arith_numeric}
\alias{numeric_arith_frab}
\alias{c_frab_add}
\alias{c_frab_multiply}
\alias{c_frab_eq}
\alias{c_frab_pmax}
\alias{c_frab_identity}
\title{Arithmetic methods for class \code{"frab"}}
\description{

  The \code{frab} class provides basic arithmetic methods for frab
  objects.  Low-level helper functions \code{c_frab_eq()} amd
  \code{c_frab_pmax()} are documented here for consistency; but
  technically \code{c_frab_eq()} is a Comparison operator, and
  \code{c_frab_pmax()} is an \dQuote{Extremes} function.  They are
  documented at \code{Compare.Rd} and \code{pmax.Rd} respectively.
  
}
\usage{
frab_negative(x)
frab_reciprocal(x)
frab_plus_frab(F1,F2)
frab_multiply_numeric(e1,e2)
frab_power_numeric(e1,e2)
numeric_power_frab(e1,e2)
frab_unary(e1,e2)
frab_arith_frab(e1,e2)
frab_plus_numeric(e1,e2)
frab_arith_numeric(e1,e2)
numeric_arith_frab(e1,e2)
}
\arguments{
  \item{e1,e2,x,F1,F2}{Objects of class \code{frab}, coerced if needed}
}
\section{Methods}{
  \describe{
    \item{Arith}{\code{signature(e1="frab"   , e2="missing")}: blah blah
    blah}
    \item{Arith}{\code{signature(e1="frab"   , e2="frab"   )}: ... }
    \item{Arith}{\code{signature(e1="frab"   , e2="numeric")}: ... }
    \item{Arith}{\code{signature(e1="numeric", e2="frab"   )}: ... }
    \item{Arith}{\code{signature(e1="ANY"    , e2="frab"   )}: ... }
    \item{Arith}{\code{signature(e1="frab"   , e2="ANY"    )}: ... }
  }
}
\author{Robin K. S. Hankin}
\value{Return \code{frab} objects}
\seealso{\code{\link{Compare}}}
\examples{

(x <- frab(c(a=1,b=2,c=3)))
(y <- frab(c(b=-2,d=8,x=1,y=7)))
(z <- frab(c(c=2,x=5,b=1,a=6)))


x+y
x+y+z

x*y


}

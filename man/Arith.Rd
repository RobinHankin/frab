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
\alias{frab_unary}
\alias{frab_arith_frab}
\alias{frab_arith_numeric}
\alias{numeric_arith_frab}
\alias{c_frab_add}
\alias{c_frab_identity}
\title{Extraction and replacement methods for class \code{"frab"}}
\description{

  The \code{frab} class provides basic arithmetic methods for frab
  objects.
  
}
\usage{
frab_negative(x)
frab_reciprocal(x)
frab_plus_frab(F1,F2)
frab_multiply_numeric(e1,e2)
frab_power_numeric(e1,e2)
numeric_multiply_frab(e1,e2)
numeric_power_frab(e1,e2)
frab_unary(e1,e2)
frab_arith_frab(e1,e2)
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
\examples{

x <- frab(c(a=1,b=2,c=3))
y <- frab(c(b=-2,d=8))

x+y


}
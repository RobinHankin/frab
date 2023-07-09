\name{Ops}
\alias{ops}
\alias{frab_negative}
\alias{frab_plus_frab}
\alias{frab_multiply_numeric}
\alias{frab_power_numeric}
\alias{numeric_multiply_frab}
\alias{numeric_power_frab}
\alias{frab_unary}
\alias{frab_arith_frab}
\alias{frab_arith_numeric}
\alias{numeric_arith_frab}
\alias{c_frab_add}
\alias{c_frab_identity}
\title{Arithmetic operators in the frab package}
\description{
  Allows arithmetic operators to be used for \code{frab} objects.
}
\usage{
\method{Ops}{frab}(e1, e2)
frab_negative(x)
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
\details{
  The function \code{Ops.frab()} passes unary and binary arithmetic
  operators \dQuote{\code{+}}, \dQuote{\code{-}}, \dQuote{\code{*}},
  \dQuote{\code{/}} and \dQuote{\code{^}} to the appropriate specialist
  function.  The package has the standard binary operators.
}
\value{Generally return \code{frab} objects.}
\author{Robin K. S. Hankin}
\examples{

x <- frab(c(a=1, b=2, c=5))
y <- frab(c(b=-2, c=1, d=8))

x+y
}


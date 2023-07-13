\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,frab,frab-method}
\alias{Compare,frab,numeric-method}
\alias{Compare,numeric,frab-method}
\alias{frab_eq}
\alias{frab_compare_frab}
\alias{frab_eq_num}
\alias{frab_gt_num}
\alias{frab_ge_num}
\alias{frab_lt_num}
\alias{frab_le_num}
\alias{frab_compare_numeric}
\alias{num_eq_frab}
\alias{num_gt_frab}
\alias{num_ge_frab}
\alias{num_lt_frab}
\alias{num_le_frab}
\alias{numeric_compare_frab}
\title{Comparision methods}
\description{
  Methods for comparison (greater than, etc) in the \pkg{frab} package.

  Functions \code{frab_gt_num()} etc follow a consistent naming
  convention; the mnemonic is the old Fortran \code{.GT.} scheme [for
  \dQuote{greater than}].

  Function \code{frab_eq()} is an odd-ball, formally documented at
  \code{Arith.Rd}.  It is slightly different from the other comparisons:
  it calls low-level helper function \code{c_frab_eq()}, which calls its
  \proglang{C} namesake which is written for speed (specifically,
  returning \code{FALSE} as soon as it spots a difference between its
  two arguments).
  
}
\usage{
frab_eq(e1,e2)
frab_compare_frab(e1,e2)
frab_eq_num(e1,e2)
frab_gt_num(e1,e2)
frab_ge_num(e1,e2)
frab_lt_num(e1,e2)
frab_le_num(e1,e2)
frab_compare_numeric(e1,e2)
num_eq_frab(e1,e2)
num_gt_frab(e1,e2)
num_ge_frab(e1,e2)
num_lt_frab(e1,e2)
num_le_frab(e1,e2)
numeric_compare_frab(e1,e2)
}

\arguments{
 \item{e1,e2}{Objects of class \code{frab}} 
}
\author{Robin K. S. Hankin}
\value{Generally, return a \code{frab} or a logical}
\seealso{\code{\link{Arith}}}
\examples{
rfrab()
a <- rfrab(26,sym=letters)
a[a<4] <- 100
}
\keyword{methods}
\keyword{math}

\name{Compare-methods}
\docType{methods}
\alias{Compare-methods}
\alias{Compare,frab,frab-method}
\alias{Compare,frab,numeric-method}
\alias{Compare,numeric,frab-method}
\alias{frab_eq}
\alias{frab_eq_num}
\alias{frab_ne_num}
\alias{frab_gt_num}
\alias{frab_ge_num}
\alias{frab_lt_num}
\alias{frab_le_num}
\alias{num_eq_frab}
\alias{num_ne_frab}
\alias{num_gt_frab}
\alias{num_ge_frab}
\alias{num_lt_frab}
\alias{num_le_frab}
\alias{numeric_compare_frab}
\alias{frab_compare_frab}
\alias{frab_compare_numeric}
\alias{c_frab_eq}
\alias{Compare}
\title{Comparison methods}
\description{
  Methods for comparison (greater than, etc) in the \pkg{frab} package.

  Frabs and sparsetables may be compared with length-one numeric
  vectors.  Functions \code{frab_gt_num()} etc follow a consistent
  naming convention; the mnemonic is the old Fortran \code{.GT.} scheme
  [for \dQuote{greater than}].  This allows one to use idiom such as
  \code{f >= 3}.  For sparsetables, comparison with scalars is possible:
  but the result is flattened to a \code{disord} object (this can be
  confusing for two dimensional tables when the default matrix-like
  print method is used, because zero entries are not \dQuote{real}.  For
  example, if \code{s} is a sparsetable, then \code{s==0} will return
  all \code{FALSE}).

  Comparing a \code{frab} with another \code{frab} is generally
  meaningless.  Idiom like \dQuote{\code{e1 >= e2}}, for example,
  returns an error.  The only comparison that makes any sense is whether
  two frabs are identical: this is detected by \dQuote{\code{e1 == e2}}
  and its negation \dQuote{\code{e1 != e2}}.  Internally, equality is
  tested in \proglang{C} using a routine written for speed
  (specifically, returning \code{FALSE} as soon as it spots a difference
  between its two arguments); this is modelled on its equivalent in the
  \CRANpkg{spray} package.  If any value is \code{NA}, equality checks
  will return \code{FALSE}.  Functions \code{frab_eq()} and
  \code{c_frab_eq()} are just \proglang{R} wrappers for the \proglang{C}
  routine \code{equal()}.

}

\usage{
frab_eq(e1,e2)
frab_eq_num(e1,e2)
frab_ne_num(e1,e2)
frab_gt_num(e1,e2)
frab_ge_num(e1,e2)
frab_lt_num(e1,e2)
frab_le_num(e1,e2)
num_eq_frab(e1,e2)
num_ne_frab(e1,e2)
num_gt_frab(e1,e2)
num_ge_frab(e1,e2)
num_lt_frab(e1,e2)
num_le_frab(e1,e2)
numeric_compare_frab(e1,e2)
frab_compare_frab(e1,e2)
frab_compare_numeric(e1,e2)
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

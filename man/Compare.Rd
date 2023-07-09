\name{compare}
\alias{compare}
\alias{Compare,frab,missing-method}
\alias{Compare,frab,frab-method}
\alias{Compare,frab,frab-method}
\alias{Compare,frab,numeric-method}
\alias{Compare,ANY,frab-method}
\alias{Compare,frab,ANY-method}
\alias{frab_compare_error}
\alias{frab_compare_frab}
\alias{frab_compare_numeric}
\alias{frab_eq}
\alias{frab_eq_num}
\alias{frab_ge_num}
\alias{frab_gt_num}
\alias{frab_le_num}
\alias{frab_lt_num}
\alias{num_eq_frab}
\alias{num_ge_frab}
\alias{num_gt_frab}
\alias{num_le_frab}
\alias{num_lt_frab}
\alias{numeric_compare_frab}
\title{Comparison methods for class \code{"frab"}}
\description{
  The \code{frab} class provides basic arithmetic and compare/replace
  methods for frab objects.

  Class \dfn{index} is taken from the excellent \CRANpkg{Matrix} package
  and is a \code{setClassUnion()} of classes \code{numeric},
  \code{logical}, and \code{character}.
  
}
\section{Methods}{
  \describe{
    \item{[}{\code{signature(x = "frab", i = "character", j = "missing")}: ... }
    \item{[}{\code{signature(x = "frab", i = "disord", j = "missing")}: ... }
    \item{[}{\code{signature(x = "frab", i = "missing", j = "missing")}: ... }
    \item{[<-}{\code{signature(x = "frab", i = "character",j = "missing", value = "ANY")}: ... }
    \item{[<-}{\code{signature(x = "frab", i = "ANY",j = "ANY", value = "ANY")}: ... }
}
}
\author{Robin K. S. Hankin}
\examples{

x <- frab(c(a=1,b=2,c=3))
y <- frab(c(b=-2,d=8))

x+y


}

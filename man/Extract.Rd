\name{extract}
\alias{extract}
\alias{[}
\alias{[.frab}
\alias{[,frab-method}
\alias{[,frab,ANY,ANY-method}
\alias{[,frab,missing,index-method}
\alias{[,frab,frab,missing-method}
\alias{[,frab,frab,missing,ANY-method}
\alias{[,frab,missing,missing-method}
\alias{[,frab,ANY,missing,ANY-method}
\alias{[,frab,character,missing,ANY-method}
\alias{[,frab,disord,missing,ANY-method}
\alias{[,ANY,frab,ANY-method}
\alias{[,frab,missing,missing,ANY-method}
\alias{[<-}
\alias{[.frab}
\alias{[,frab-method}
\alias{[,frab,character,missing-method}
\alias{[,frab,disord,missing-method}
\alias{[,frab,missing,missing-method}
\alias{[,frab,missing,missing,ANY-method}
\alias{[<-,frab,character,missing-method}
\alias{[<-,frab,ANY,ANY,ANY-method}
\title{Extraction and replacement methods for class \code{"frab"}}
\description{
  The \code{frab} class provides basic arithmetic and extract/replace
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

   Double square extraction, as in \code{x[[i]]} and \code{x[[i]] <-
  value}, is not currently defined.
  
}
\author{Robin K. S. Hankin}
\examples{

x <- frab(c(a=1,b=2,c=3))
y <- frab(c(b=-2,d=8))

x+y


}

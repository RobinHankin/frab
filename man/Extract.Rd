\name{Extract}
\alias{extract}
\alias{values,frab-method}
\alias{values}
\alias{values<-}
\alias{values<-,frab,numeric-method}
\alias{values<-,frab,disord-method}
\alias{names}
\alias{names,frab-method}
\alias{names<-,frab,disord-method}
\alias{names<-,frab,character-method}
\alias{[}
\alias{[.frab}
\alias{[,frab-method}
\alias{[,ANY,frab,ANY-method}
\alias{[,frab,ANY,ANY-method}
\alias{[,frab,ANY,missing,ANY-method}
\alias{[,frab,character,missing,ANY-method}
\alias{[,frab,character,missing-method}
\alias{[,frab,disindex,missing,ANY-method}
\alias{[,frab,disord,missing,ANY-method}
\alias{[,frab,disord,missing-method}
\alias{[,frab,frab,missing-method}
\alias{[,frab,frab,missing,ANY-method}
\alias{[,frab,missing,missing-method}
\alias{[,frab,missing,missing,ANY-method}
\alias{[,frab,missing,index-method}
\alias{[<-}
\alias{[<-,frab,ANY,ANY,ANY-method}
\alias{[<-,frab,character,missing-method}
\alias{[<-,frab,character,missing,numeric-method}
\alias{[<-,frab,character,missing,logical-method}
\alias{[<-,frab,disord,missing-method}
\alias{[<-,frab,disord,missing,frab-method}
\alias{[<-,frab,disord,missing,logical-method}
\alias{[<-,frab,disord,missing,numeric-method}
\alias{[<-,frab,disindex,missing,numeric,ANY-method}
\alias{[<-,frab,disindex,missing,numeric-method}
\alias{[<-,frab,missing,missing,frab-method}
\alias{[<-,frab,missing,missing,numeric-method}
\alias{[<-,frab,missing,missing,ANY-method}
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
    \item{[}{\code{signature(x = "frab", i = "character", j =
	"missing")}: \code{x["a"] <- 33}}
    \item{[}{\code{signature(x = "frab", i = "disord", j = "missing")}: \code{x[x>3]}}
    \item{[}{\code{signature(x = "frab", i = "missing", j = "missing")}: \code{x[]}}
    \item{[<-}{\code{signature(x = "frab", i = "character",j =
	"missing", value = "ANY")}: \code{x["a"] <- 3}}
    \item{[<-}{\code{signature(x = "frab", i = "disord", j = "missing",
	value="frab")}: \code{x[x<0] <- -x[x<0]}; not implemented}
    \item{[<-}{\code{signature(x = "frab", i = "disord", j = "missing",
	value="logical")}: \code{x[x<0] <- NA}}
    \item{[<-}{\code{signature(x = "frab", i = "ANY",j = "ANY", value =
	"ANY")}: not implemented}
    \item{[<-}{\code{signature(x = "frab", i = "disindex",j = "missing",
	value = "numeric")}: \code{x[x>0] <- 3}}
    \item{[<-}{\code{signature(x = "frab", i = "character", j =
	"missing", value = "logical")}: \code{x["c"] <- NA}}
}

 Double square extraction, as in \code{x[[i]]} and \code{x[[i]] <-
 value}, is not currently defined.  In replacement methods, if
 \code{value} is logical it is coerced to numeric (this includes
 \code{NA}).

 Special dispensation is given for extraction of a \code{frab} with a
 length zero index, as in \code{x[NULL]}, which returns the empty
 \code{frab} object.
  
}
\author{Robin K. S. Hankin}
\value{Generally, return a \code{frab} object.}
\examples{

frab(setNames(seq_len(0),letters[seq_len(0)]))

a <- rfrab(26,sym=letters)
a<4
a[a<4]
a[a<4] <- 100
a

x <- rfrab()
values(x) <- values(x) + 66

x <- rfrabb()
v <-  values(x)
v[v<0] <- abs(v[v<0]) + 50
values(x) <- v

names(x) <- toupper(names(x))
x

}


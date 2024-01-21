\name{rfrab}
\alias{rfrab}
\alias{rfrabb}
\alias{rfrabbb}
\title{Random frabs}
\description{Random \code{frab} objects, intended as quick
  \dQuote{get you going} examples}
\usage{
rfrab(n = 9, v = seq_len(5), symb = letters[seq_len(9)])
rfrabb(n = 100, v = -5:5, symb = letters)
rfrabbb(n = 5000, v = -10:10, symb = letters, i=3)
}
\arguments{
  \item{n}{Length of object to return}
  \item{v}{Values to assign to symbols (see details)}
  \item{symb}{Symbols to use}
  \item{i}{Exponentiating index for \code{rfrabbb()}.  Symbols in
    returned value will be \code{i} concatenated elements of \code{symb}
  }
}
\details{
  What you see is what you get, basically.  If a symbol is chosen more
  than once, as in, \code{c(a=1,b=2,a=3)}, then the value for \code{a}
  will be summed.

  Use function \code{rfrab()} for a small, easily-managed object;
  \code{rfrabb()} and \code{rfrabbb()} give successively larger objects.

}
\value{Returns a frab object}
\author{Robin K. S. Hankin}
\examples{
rfrab()
}

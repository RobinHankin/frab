\name{rfrab}
\alias{rfrab}
\alias{rfreealg}
\title{Random frabs}
\description{Random \code{frab} objects, intended as quick
  \dQuote{get you going} examples}
\usage{
rfrab(n = 9, v = seq_len(5), symb = letters[seq_len(9)])
}
\arguments{
  \item{n}{Length of object to return}
  \item{v}{Values to assign to symbols (see details)}
  \item{symb}{Symbols to use}
}
\details{
  What you see is what you get, basically.  If a symbol is chosen more
  than once, as in, \code{c(a=1,b=2,a=3)}, then the value for \code{a}
  will be summed.
}
\author{Robin K. S. Hankin}
\examples{
rfrab()
}

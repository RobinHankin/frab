\name{pmax}
\alias{pmax}
\alias{pmin}
\alias{pmax.frab}
\alias{pmin.frab}
\alias{pmax,frab-method}
\alias{pmin,frab-method}
\alias{pmax,ANY-method}
\alias{pmin,ANY-method}
\alias{pmax_pair}
\alias{pmin_pair}
\alias{pmax_dots}
\alias{pmin_dots}
\title{Parallel maxima and minima for frabs}
\description{
Parallel (pairwise) maxima and minima for frabs. 
}
\usage{
pmax_pair(F1,F2)
pmin_pair(F1,F2)
pmax_dots(x, ...)
pmin_dots(x, ...)
\S4method{pmax}{frab}(...)
\S4method{pmin}{frab}(...)
}
\arguments{
  \item{F1, F2, x, ...}{Frab objects}
}
\details{

Pairwise minima and maxima for frabs, using names as the primary key.

Functions \code{pmax_pair()} calls \code{c_frab_pmax()} and \code{pmin_pair()} use 


Functions \code{pmax()} and \code{pmin()} use the same mechanism as
\code{cbrob()} of the \CRANpkg{Brobdingnag} package, originally due to
John Chambers (pers. comm.)
  
}
\value{Returns a frab object}
\author{Robin K. S. Hankin}
\examples{

x <- rfrab()
y <- rfrab()

}

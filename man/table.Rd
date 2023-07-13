\name{table}
\alias{table}
\alias{is.1dtable}
\alias{table_to_frab}
\alias{as.table}
\alias{as.table,frab-method}
\title{Tables and frab objects}
\description{
  Various methods and functions to deal with tables in the \pkg{frab}
  package. 
}
\usage{
\S4method{as.table}{frab}(x,...)
table_to_frab(x)
}
\arguments{
  \item{x}{Object of class \code{frab} or \code{table}}
  \item{...}{Further arguments, currently ignored}
}
\details{

  If a \code{frab} object has non-negative entries it may be interpreted
  as a table.  However, in base \R, \code{table} objects do not have
  sensible addition methods which is why the \pkg{frab} package is
  needed.

  Function \code{is.1dtable()} checks for its argument being a
  one-dimensional table.  The idea is that a table like
  \code{table(sample(letters,30,TRUE))}, being a table of a single
  observation, is accepted but a table like
  \code{table(data.frame(rnorm(20)>0,rnorm(20)>0))} is not acceptable
  because it is a \emph{two}-dimensional contingency table.


}
\value{Generally return a table or frab.}
\author{Robin K. S. Hankin}
\note{
The order of the entries may be changed during the coercion, as per
  \CRANpkg{disordR} discipline.  Function \code{as.frab()} takes a
  table, dispatching to \code{table_to_frab()}.
}
\examples{
X <- table(letters[c(1,1,1,1,2,3,3)])
Y <- table(letters[c(1,1,1,1,3,4,4)])
Z <- table(letters[c(1,1,2,3,4,5,5)])

X+Y  # defined but nonsense

# X+Z  # returns an error


as.frab(X) + as.frab(Y)  # correct answer

plot(as.table(rfrab()))
}

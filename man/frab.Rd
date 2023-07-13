\name{frab}
\alias{frab}
\alias{as.frab}
\alias{is.frab}
\alias{list_to_frab}
\title{Creating \code{frab} objects}
\description{Package idiom for creating \code{frab} objects}
\usage{
frab(x)
as.frab(x)
is.frab(x)
list_to_frab(L)
}
\arguments{
\item{x}{object coerced to, or tested for, \code{frab}}
\item{L}{List of two elements, a numeric vector named \code{values} and
a character vector named \code{names}}
}
\details{

Function \code{frab()} is the creation method, taking a named numeric
vector as its argument; it is the only function in the package that
actually calls \code{new("frab", ...)}.

Function \code{as.frab()} tries a bit harder to be useful and can coerce
different types of object to a \code{frab}.  If given a list it
dispatches to \code{list_to_frab()}.  If given a table it dispatches to
\code{table_to_frab()}, documented at \code{table.Rd}.
}
\value{Returns a \code{frab}, or a boolean}
\author{Robin K. S. Hankin}
\seealso{\code{\link{frab-class}}}
\examples{
as.frab(c(a=2,b=1,c=77))

as.frab(list(names=letters[5:2],values=1:4))


}

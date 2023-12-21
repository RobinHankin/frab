\name{dataframe}
\alias{dataframe}
\alias{data.frame}
\alias{df_to_frab}
\alias{as.data.frame}
\alias{as.data.frame,frab-method}
\alias{as.frab,data.frame-method}
\title{Coerce a data frame to a frab}
\description{Coerce a data frame to a frab}
\usage{
df_to_frab(from)
\S4method{as.frab}{data.frame}(x)
}
\arguments{
  \item{x, from}{Frab objects}
}
\details{

  Coerces a data frame, with columns \code{key} and \code{value}, to the
  appropriate frab object.  Repeated \code{key}s are summed.
  
}
\value{Returns a frab object or a dataframe.}
\author{Robin K. S. Hankin}
\examples{

as.frab(data.frame(key=letters[1:5],value=1:5))

}

% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{addColumn}
\alias{addColumn}
\title{Add a column to an oce object}
\description{Add a column to an oce object's \code{data}.}
\usage{addColumn(x, data, name)}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{data}{the data.  The length of this item must match that of the
      existing data entries in the \code{data} slot).}
  \item{name}{the name of the column.}
}

\details{If there is already a column with the given name, its contents are
  replaced by the new value.}

\value{An object of \code{\link[base]{class}} \code{oce}, with a new column.}

\seealso{\code{\link{ctdAddColumn}} does a similar thing for \code{ctd} objects,
  and is in fact called, if \code{x} is of class \code{ctd}.}

\examples{
library(oce)
data(ctd) 
st <- swSigmaTheta(ctd[["salinity"]], ctd[["temperature"]], ctd[["pressure"]])
new <- addColumn(ctd, st, "sigmaTheta")
}

\author{Dan Kelley}

\keyword{misc}


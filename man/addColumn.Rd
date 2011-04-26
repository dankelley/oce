\name{addColumn}
\alias{addColumn}
\title{Add a column to an oce object}
\description{Add a column to an oce object's \code{data}.}
\usage{addColumn(x, data, name)}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{data}{the data (length must match length of columns in existing
    \code{x$data} data frame).}
  \item{name}{the name of the column.}
}

\details{The header is unchanged by this operation.}

\value{An object of \code{\link[base]{class}} \code{oce}, with a new column.}

\seealso{\code{add.ctd.column} does a similar thing for \code{ctd}
objects.}

\examples{
library(oce)
data(ctd) 
st <- swSigmaTheta(ctd$data$salinity, ctd$data$temperature, ctd$data$pressure);
new <- addColumn(ctd, st, "sigmatheta")
}

\author{Dan Kelley}
\keyword{misc}

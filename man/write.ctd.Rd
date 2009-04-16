\name{write.ctd}
\alias{write.ctd}
\title{Write a CTD data object as a .csv file}

\description{Write a CTD data object as a \code{.csv} file.}

\usage{write.ctd(object, file=stop("'file' must be specified"))}

\arguments{
  \item{object}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{file}{Either a character string (the file name) or a connection.  This is a mandatory argument.}
}

\details{Writes a comma-separated file containing the data frame stored
  in \code{object$data}.  The file is suitable for reading with a
  spreadsheet, or with \code{\link{read.csv}}.  Note that the output
  file will retain none of the meta-data stored in \code{object}.}

\examples{
library(oce)
data(ctd)
write.ctd(ctd, "ctd.csv")
x <- read.csv("ctd.csv")
plot(x$salinity, x$temperature)
plot.TS((as.ctd(x$salinity, x$temperature,0)))
}

\author{Dan Kelley}
\keyword{misc}

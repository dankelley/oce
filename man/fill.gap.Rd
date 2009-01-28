\name{fill.gap}

\alias{fill.gap}

\title{Fill a gap in an oce object}

\description{Fill a gap in an oce object}

\usage{fill.gap(x, start, end, column)}

\arguments{
  \item{x}{an \code{oce} object.}
  \item{start}{the start of the bad section of data (see details).}
  \item{end}{the start of the bad section of data (see details).}
  \item{column}{if provided, the name of the column referred to by
    \code{start} and \code{end}.}
}

\details{Gaps in the data (i.e. columns in \code{x$data}) are filled by
  linear interpolation between the \code{start} and \code{end} points.
  If \code{column} is not provided, then \code{start} and \code{end} are
  indices.  If \code{column} is provided, then \code{start} and
  \code{end} refer to values in the named column.}

\value{A new \code{oce} object.}

\examples{
library(oce)
data(tdr)
d <- subset(tdr, as.POSIXct("2008-06-26") <= t & t <= as.POSIXct("2008-06-27"))
plot(d$data$t, d$data$temperature, type="l", lwd=2, xlab="t", ylab="Temperature [degC]")
dd <- fill.gap(d, as.POSIXct("2008-06-26 09:00:00"), as.POSIXct("2008-06-26 20:00:00"), "t")
lines(dd$data$t, dd$data$temperature, col="red")
abline(v=as.POSIXct("2008-06-26 09:00:00"), col="pink")
abline(v=as.POSIXct("2008-06-26 20:00:00"), col="pink")
}

\author{Dan Kelley}

\keyword{misc}

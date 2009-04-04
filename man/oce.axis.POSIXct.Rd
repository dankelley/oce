\name{oce.axis.POSIXct}

\alias{oce.axis.POSIXct}

\title{Modified version of axis.POSIXct}

\description{Modified version of axis.POSIXct}

\usage{oce.axis.POSIXct(side, x, at, format, labels = TRUE, ...)}

\arguments{
  \item{side}{as for \code{\link{axis.POSIXct}}.}
  \item{x}{as for \code{\link{axis.POSIXct}}.}
  \item{at}{as for \code{\link{axis.POSIXct}}.}
  \item{format}{as for \code{\link{axis.POSIXct}}.}
  \item{labels}{as for \code{\link{axis.POSIXct}}.}
  \item{\dots}{as for \code{\link{axis.POSIXct}}.}
}

\details{As \code{\link{axis.POSIXct}} but with axis labels obeying the
  timezone of \code{x}.  This will not be needed for \R 2.9 and later,
  but is included so that \code{oce} will work even with earlier versions.}

\value{The locations on the axis scale at which tick marks were drawn.}

\seealso{\code{axis.POSIXct}, from which this is derived.}

\author{Dan Kelley}

\keyword{misc}

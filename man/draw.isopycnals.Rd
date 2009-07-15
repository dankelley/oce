\name{draw.isopycnals}
\alias{draw.isopycnals}
\title{Draw isopycnal curves on an existing temperature-salinity plot}
\description{Draw isopycnal curves on an existing temperature-salinity plot}
\usage{draw.isopycnals(rho.levels=6, rotate.rho.labels=TRUE, rho1000=FALSE, cex=1, col="darkgray")}
\arguments{
  \item{rho.levels}{suggested number of density levels (i.e. isopycnal curves).}
  \item{rotate.rho.labels}{boolean, set to \code{TRUE} to write all density labels horizontally.}
  \item{rho1000}{boolean, set to \code{TRUE} to write isopycnal labels as e.g. 1024 instead of 24.}
  \item{cex}{size for labels.}
  \item{col}{colour for lines and labels.}
}

\details{Adds isopycnal lines to an existing temperature-salinity plot.
  This is called by \code{\link{plot.TS}}, and may be called by the user
  also, e.g. if an image plot is used to show TS data density.}

\value{None.}

\seealso{\code{\link{plot.TS}}, which calls this.}

\author{Dan Kelley}
\keyword{misc}

\name{drawIsopycnals}
\alias{drawIsopycnals}
\title{Draw isopycnal curves on an existing temperature-salinity plot}
\description{Draw isopycnal curves on an existing temperature-salinity plot}

\usage{
drawIsopycnals(rhoLevels=6, rotateRhoLabels=TRUE, rho1000=FALSE, eos=getOption("eos"),
               cex=1, col="darkgray", lwd=par("lwd"), lty=par("lty"))
}

\arguments{
  \item{rhoLevels}{suggested number of density levels (i.e. isopycnal curves).}
  \item{rotateRhoLabels}{boolean, set to \code{TRUE} to write all density labels horizontally.}
  \item{rho1000}{boolean, set to \code{TRUE} to write isopycnal labels as e.g. 1024 instead of 24.}
  \item{eos}{name of equation of state to be used, either \code{"unesco"} or
      \code{"teos"}.  If the latter, then the computer must have the TEOS
      library installed; see \code{\link{teos}}.}
  \item{cex}{size for labels.}
  \item{col}{colour for lines and labels.}
  \item{lwd}{line width for isopcynal curves}
  \item{lty}{line type for isopcynal curves}
}

\details{Adds isopycnal lines to an existing temperature-salinity plot.
  This is called by \code{\link{plotTS}}, and may be called by the user
  also, e.g. if an image plot is used to show TS data density.}

\value{None.}

\seealso{\code{\link{plotTS}}, which calls this.}

\author{Dan Kelley}
\keyword{misc}

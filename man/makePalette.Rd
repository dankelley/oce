\name{makePalette}

\alias{makePalette}

\title{Make a palette}

\description{Make a palette, e.g. for bathymetric images}

\usage{makePalette(type="topography", style=c("gmt0"), file, breaksPerLevel=16)}

\arguments{

  \item{type}{a string indicating the type of palette (only one choice at
      present)}

  \item{style}{a string indicating the style of palette (see \sQuote{Details}),
      ignored if \code{file} is given.  \code{gmt0} yields the GMT palette
      defined in the GMT file named \code{GMT_relieve.cpt}.}

  \item{file}{optional name of a file containing palette information in GMT
      format (see \sQuote{References})}

  \item{breaksPerLevel}{number of breaks between each level of the palette}
}

\details{This function is in a preliminary form (for testing), and its
    arguments and action may change in the early months of 2014.}

\value{A list containing \code{breaks} and \code{col}, as used by e.g.
    \code{\link{image}} or \code{\link{imagep}}.}

\examples{
library(oce)
pal <- makePalette()
data(topoWorld)
imagep(topoWorld, breaks=pal$breaks, col=pal$col, asp=1)
}

\references{The GMT documentation is at (\url{http://gmt.soest.hawaii.edu}, and
    files suitable for the use here are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}


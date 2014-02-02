\name{makePalette}

\alias{makePalette}

\title{Make a palette}

\description{Make a palette, e.g. for bathymetric images}

\usage{
makePalette(style=c("gmt_relief", "gmt_ocean", "oce_shelf"),
            file, breaksPerLevel=20,
            region=c("water", "land", "both"))
}

\arguments{

  \item{style}{a string indicating the style of palette (see \sQuote{Details}),
      ignored if \code{file} is given.  Lower-case names are used for GMT
      colour palettes, e.g. \code{gmt_relief} uses information in the GMT
      definition file named \code{GMT_relief.cpt}.}

  \item{file}{optional name of a file containing palette information in GMT
      format (see \sQuote{References})}

  \item{breaksPerLevel}{number of breaks between each level of the palette, in
      a linear mapping from level to level.  Set to 1 to get a staircase
      mapping.}

  \item{region}{indicate region (with water meaning negative topographic
      values.)}

}

\details{This function is in a preliminary form (for testing), and its
    arguments and action may change in the early months of 2014.}

\value{A list containing \code{breaks} and \code{col}, as used by e.g.
    \code{\link{image}} or \code{\link{imagep}}.}

\section{Development status}{Devised 2014-Jan-31 with a particular plot in
    mind, this function is likely to be somewhat unstable until perhaps the end
    of Feb, 2014.}

\examples{
library(oce)
data(topoWorld)
pal <- makePalette(region="both")
imagep(topoWorld, breaks=pal$breaks, col=pal$col, asp=1)

## (test) shelf scheme
source('~/src/R-kelley/oce/R/imagep.R')
pal <- makePalette(style="oce_shelf", breaksPerLevel=2)
N <- 100 # increase to make breaks match exactly contours
x <- 1:N
y <- 1:N
z <- outer(x, y, function(x,y) -200*(x-1)/N)
imagep(x, y, z, breaks=pal$breaks, col=pal$col)
contour(x, y, z, add=TRUE, col='red', labcex=1, levels=-standardDepths()[1:10])


}

\references{The GMT documentation is at (\url{http://gmt.soest.hawaii.edu}, and
    files suitable for the use here are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}


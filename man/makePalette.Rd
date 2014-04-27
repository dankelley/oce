\name{makePalette}

\alias{makePalette}

\title{Make a palette [DEPRECATED]}

\description{Make a palette, e.g. for bathymetric images.  DEPRECATION NOTICE:
    this function will be replaced by \code{\link{colormap}} by early May of
    2014.}

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

\details{This function is in a preliminary form (for testing); see also
    \code{\link{colorize}}, a related function that may eventually provide
    similar functionality to this one.}

\value{A list containing \code{breaks} and \code{col}, as used by e.g.
    \code{\link{image}} or \code{\link{imagep}}.}

\section{Development status}{Devised in January 2014 with a particular plot in
    mind, this function is likely to undergo changes in both arguments and
    return values, until perhaps mid-summer, 2014.}

\examples{
library(oce)
data(topoWorld)
pal <- makePalette(region="both")
imagep(topoWorld, breaks=pal$breaks, col=pal$col, asp=1)

## (test) shelf scheme
pal <- makePalette(style="oce_shelf", breaksPerLevel=1)
N <- 300 # increase to make breaks match exactly contours
x <- 1:N
y <- 1:5
z <- outer(x, y, function(x,y) -300*(x-1)/N)
imagep(x, y, z, breaks=pal$breaks, col=pal$col)
contour(x, y, z, add=TRUE, col='red', labcex=1,
        levels=-c(seq(0, 200, 25), 250, 300))
}

\references{The GMT documentation is at (\url{http://gmt.soest.hawaii.edu}, and
    files suitable for the use here are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}


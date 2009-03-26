\name{oce.colors}

\alias{oce.colors.jet}
\alias{oce.colors.gebco}
\alias{oce.colors.two}
\alias{oce.colors.palette}

\title{Create a palette of colours}

\description{Create a palette of colours}

\usage{
oce.colors.palette(n, which=1)

oce.colors.gebco(n=9, region=c("water", "land", "both"),
	type=c("fill","line"))

oce.colors.jet(n)

oce.colors.two(n, low=2/3, high=0, smax=1, alpha = 1)

}

\arguments{
\item{n}{the number of colors (\eqn{\ge 1}{>=1}) to be in the palette.}
\item{which}{an identifier for the palette, for \code{oce.colors.palette}.}
\item{region}{the region characteristic, for \code{oce.colors.gebco}.}
\item{type}{the type of item drawn, for \code{oce.colors.gebco}.}
\item{low}{the hue, in [0,1], for the low end of a \code{oce.colors.two}
  scale.}
\item{high}{the hue, in [0,1], for the high end of a \code{oce.colors.two}
  scale.}
\item{smax}{the maximum saturation, in [0,1], for the colors of \code{oce.colors.two}.}
\item{alpha}{the alpha value, in [0,1], for the colors of \code{oce.colors.two}.}
}

\details{

  \code{oce.colors.palette} provides a variety of pre-defined palettes,
  e.g. from ColorBrewer.

  \code{oce.colors.gebco} provides palettes that mimic the GEBCO altas
  colours, with shades of blue for water and of brown for land.

  \code{oce.colors.jet} provides a palette similar to the Matlab
  \dQuote{jet} palette.

  \code{oce.colors.two} provides a two-tone palette that fades to white
  at central values.
}

\examples{
library(oce)
x <- array(1:1000, dim=c(1,1000))
par(mfrow=c(1,4))
image(x, col=oce.colors.two(100), main="oce.colors.two")
image(x, col=oce.colors.jet(100), main="oce.colors.jet")
image(x, col=oce.colors.gebco(100), main="oce.colors.gebco")
image(x, col=oce.colors.palette(100), main="oce.colors.palette")
}

\references{
Color Brewer.
http://www.personal.psu.edu/cab38/ColorBrewer/ColorBrewer.html

Light, A., and P. J. Bartlein, 2004.
The End of the Rainbow? Color Schemes for Improved Data Graphics.
\emph{Eos Trans. AGU}, \textbf{85(40)}, doi:10.1029/2004EO400002.

Martin Jakobsson, Ron Macnab, and Members of the Editorial Board, IBCAO.
Selective comparisons of GEBCO (1979) and IBCAO (2000) maps.
\url{http://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcao_gebco_comp.html}

Stephenson, David B., 2005.
Comment on ``Color schemes for improved data graphics,''
by A. Light and P. J. Bartlein.
\emph{Eos Trans. AGU}, \textbf{86(20)}.


}

\author{Dan Kelley}

\keyword{misc}

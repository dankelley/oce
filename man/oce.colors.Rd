\name{oce.colors}

\alias{oce.colors}
\alias{oceColors}

\alias{oce.colorsJet}
\alias{oceColorsJet}

\alias{oce.colorsGebco}
\alias{oceColorsGebco}

\alias{oce.colorsTwo}
\alias{oceColorsTwo}

\alias{oce.colorsPalette}
\alias{oceColorsPalette}

\alias{oce.colors9A}
\alias{oceColors9A}

\alias{oce.colors9B}
\alias{oceColors9B}

\alias{oce.colorsViridis}
\alias{oceColorsViridis}

\title{Create a palette of colours}

\description{Create a palette of colours}

\usage{
oce.colorsPalette(n, which=1)

oce.colorsGebco(n=9, region=c("water", "land", "both"),
	type=c("fill","line"))

oce.colorsJet(n)

oce.colors9A(n)

oce.colors9B(n)

oce.colorsTwo(n, low=2/3, high=0, smax=1, alpha = 1)

oce.colorsViridis(n)

}

\arguments{
\item{n}{the number of colors (\eqn{\ge 1}{>=1}) to be in the palette.}
\item{which}{an identifier for the palette  (see \dQuote{Details}).}
\item{region}{the region characteristic, for \code{oce.colorsGebco}.}
\item{type}{the type of item drawn, for \code{oce.colorsGebco}.}
\item{low}{the hue, in [0,1], for the low end of a \code{oce.colorsTwo}
  scale.}
\item{high}{the hue, in [0,1], for the high end of a \code{oce.colorsTwo}
  scale.}
\item{smax}{the maximum saturation, in [0,1], for the colors of \code{oce.colorsTwo}.}
\item{alpha}{the alpha value, in [0,1], for the colors of \code{oce.colorsTwo}.}
}

\details{

  \code{oce.colorsPalette} provides a variety of pre-defined palettes.
  \code{which}=1 yields the ColorBrewer diverging red/blue scheme while
  \code{which}=2 yields the ColorBrewer diverging RYB scheme [1].

  A family of nine-color schemes is as follows: \code{which="jet"} (or
  \code{which="9A"} or \code{which=9.01} for the Jet scheme; \code{which="9B"}
  or \code{which=9.02} for a scheme similar to Jet but omitting the green, and
  somewhat desaturating the yellow and cyan.

  \code{oce.colorsGebco} provides palettes that mimic the GEBCO atlas colours,
  with shades of blue for water and of brown for land.  The blue values go from
  dark to light, and the brown ones from light to dark; in this way,
  topographic images have light values near sea-level, and get darker in either
  deeper water or higher terrain.

  \code{oce.colorsJet} provides a palette similar to the Matlab
  \dQuote{jet} palette.

  \code{oce.colorsTwo} provides a two-tone palette that fades to white
  at central values.

  \code{oce.colorsViridis} provides a matplotlib (python) color scheme that
  became the standard in 2015; see [2]. This is a blue-yellow transition that
  is designed to reproduce well in black-and-white, and also to be
  interpretable by those with certain forms of colour blindness.

}

\examples{
library(oce)
opar <- par(no.readonly = TRUE)
x <- array(1:1000, dim=c(1,1000))
par(mfrow=c(1,5))
image(x, col=oce.colorsTwo(200), main="oce.colorsTwo")
image(x, col=oce.colorsJet(200), main="oce.colorsJet")
image(x, col=oce.colorsGebco(200), main="oce.colorsGebco")
image(x, col=oce.colorsPalette(200), main="oce.colorsPalette")
image(x, col=oce.colorsPalette(200), main="oce.colorsViridis")
par(opar)

# Acoustic-Doppler profiler data; note that plot.adp() puts makes
# zlim be symmetric about zero velocity.
data(adp)
plot(adp, which='u1')

# Contrast Jet with Viridis, using standard Volcano dataset;
# try printing the results in black and white.
par(mfrow=c(2,1))
imagep(volcano, col=oce.colorsJet)
imagep(volcano, col=oce.colorsViridis)
}

\references{
[1] Color Brewer.
\url{http://colorbrewer2.org/}

[2] A blog item on the Viridis (and related) matplotlib colorscales is at
\url{http://bids.github.io/colormap/}.

[3] Light, A., and P. J. Bartlein, 2004.
The End of the Rainbow? Color Schemes for Improved Data Graphics.
\emph{Eos Trans. AGU}, 85(40), doi:10.1029/2004EO400002.

[4] Martin Jakobsson, Ron Macnab, and Members of the Editorial Board, IBCAO.
Selective comparisons of GEBCO (1979) and IBCAO (2000) maps.
\url{http://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcao_gebco_comp.html}

[5] Stephenson, David B., 2005.
Comment on ``Color schemes for improved data graphics,''
by A. Light and P. J. Bartlein.
\emph{Eos Trans. AGU}, 86(20).
(See also \url{http://geography.uoregon.edu/datagraphics/color_scales.htm})

}

\author{Dan Kelley}

\keyword{color}

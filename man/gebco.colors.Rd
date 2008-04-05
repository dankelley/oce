\name{gebco.colors}
\alias{gebco.colors}
\title{Seawater viscosity}
\description{
Return colors as used on GEBCO charts of ocean depth.
}
\usage{gebco.colors(n=9, region=c("water", "land", "both"), type=c("fill","line"))}
\arguments{
  \item{n}{number of colors to return.}
  \item{region}{Either \code{"water"} (blue colors), \code{"land"} (brown colors), or "both" (brown, then blue).}
  \item{type}{Either \code{"fill"} (colours that look good with images, or filled contours) or "line" (colors that are darker overall, and that thus look better for lines on a white background.}
}
\details{
}
\value{A list of colors, as for \code{terrain.colors} and similar routines.}
\examples{
plot(1:10, 1:10, col=gebco.colors(10, "water", "line"), cex=5, pch=20)
}
\source{
The \code{"fill"} colours came from scanning a chart, and adding a few
lighter land colours by linear extrapolation.  The \code{"water"} and \code{"line"} colours are from 
the figure \code{IBCAO_GEBCO_comp_colortable_oc.jpg} in Jakobsson et al.
}
\references{
\emph{Selective comparisons of gebco (1979) and ibcao (2000) maps,}
Martin Jakobsson, 
Ron Macnab, and Members of the Editorial Board, IBCAO.
\url{http://www.ngdc.noaa.gov/mgg/bathymetry/arctic/ibcao_gebco_comp.html}
}
\author{Dan Kelley}
\keyword{misc}

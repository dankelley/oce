\name{read.coastline}

\alias{read.coastline}

\title{Scan a coastline data file}

\description{Read a coastline file in mapgen, matlab, or Splus format}

\usage{read.coastline(file,type=c("R","S","mapgen"),debug=getOption("oce.debug"), log.action)}

\arguments{
  \item{file}{name of file containing coastline data.}
  \item{type}{type of file, one of "R", "S", or "mapgen"}
  \item{debug}{set to TRUE to print information about the header, etc.}
  \item{log.action}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
}

\details{The S and R formats are identical, and consist of two columns,
  lon and lat, with land-jump segments separated by lines with two NAs.

The MapGen format is of the form
\preformatted{
# -b
-16.179081	28.553943
-16.244793	28.563330
}
BUG: the 'arc/info ungenerate' format is not yet understood.
}

\value{An object of \code{\link[base]{class}} \code{"coastline"}, 
which is a \code{\link[base]{list}} containing
  \item{data}{a list containing 
    \describe{
      \item{longitude}{the longitude in decimal degrees positive east of
	Greenwich.}
      \item{latitude}{the latitude in decimal degrees positive north of
	the equator.}
    }
  }
  \item{metadata}{a NULL item that may be used in a future version.}
  \item{processing.log}{A processing log, in the standard \code{oce} format.}
}

\seealso{The generic function \code{\link{read.oce}} provides an
  alternative to this.  Coastlines may be created from
  latitude/longitude pairs with \code{\link{as.coastline}}, summarized
  with \code{\link{summary.coastline}} and plotted with
  \code{\link{plot.coastline}}.  }

\examples{
\dontrun{
library(oce)
cl <- read.coastline("7404.dat")
# If no plot yet:
plot(cl)
# To add to an existing plot:
lines(cl$data$longitude, cl$data$latitude)
# Note: another trick is to do something like the following,
# to get issues of whether longitude is defined in (-180,180)
# or (0,360)
lines(cl$datas$longitude, cl$data$latitude)
lines(cl$data$longitude-360, cl$data$latitude)
}
}

\references{The NOAA site
\url{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html} is a good
source for coastline data files.}

\author{Dan Kelley}
\keyword{misc}

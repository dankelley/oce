\name{read.coastline}

\alias{read.coastline}
\alias{read.coastline.shapefile}
\alias{read.coastline.openstreetmap}

\title{Scan a coastline data file}

\description{Read a coastline file in mapgen, matlab, Splus, or shapefile
    format.}

\usage{
read.coastline(file, type=c("R","S","mapgen","shapefile","openstreetmap"),
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
read.coastline.shapefile(file, lonlim=c(-180,180), latlim=c(-90,90),
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
read.coastline.openstreetmap(file, lonlim=c(-180,180), latlim=c(-90,90),
  debug=getOption("oceDebug"), monitor=FALSE, processingLog)
}

\arguments{
  \item{file}{name of file containing coastline data.}
  \item{type}{type of file, one of \code{"R"}, \code{"S"}, \code{"mapgen"},
      \code{"shapefile"} or \code{"openstreetmap"}.}
  \item{debug}{set to TRUE to print information about the header, etc.}
  \item{latlim}{range of (signed) latitudes, used only for shapefiles.
    Regions that do not intersect this range are skipped.}
  \item{lonlim}{as \code{latlim}, but a signed longitude.}
  \item{monitor}{print a dot for every coastline segment read (ignored
    except for reading "shapefile" type)}
  \item{processingLog}{if provided, the action item to be stored in the
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
  \item{processingLog}{A processingLog of processing, in the standard \code{oce} format.}
}

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\examples{
\dontrun{
library(oce)
cl <- read.coastline("7404.dat")
# If no plot yet:
plot(cl)
# To add to an existing plot:
lon <- longitude(cl)
lat <- latitude(cl)
lines(lon, lat)
# Note: another trick is to do something like the following,
# to get issues of whether longitude is defined in (-180,180)
# or (0,360)
lines(lon, lat)
lines(lon-360, lat)
}
}

\section{A hack for depth contours}{
    The following demonstrates that this code is getting close to working with
    depth contours.  But this should be handled more internally, and a new
    object for depth contours should be constructed, of which coastlines could be a subset.
    \preformatted{
        library(oce)
        d <- read.coastline.shapefile("~/Dropbox/DepthContours/DepthContours.shp")
        isna <- is.na(d[["latitude"]])
        idx<-1+cumsum(isna)
        lat<-split(d[["latitude"]][!isna], idx[!isna])
        lon<-split(d[["longitude"]][!isna], idx[!isna])
        depths <- d[["depths"]]
        n <- length(depths)
        D<-200
        plot(c(-180,180),c(-90,90), xlab="", ylab="", asp=1, type='n')
        for (i in 1:n) {
            if (depths[i]==D)
                lines(lon[[i]],lat[[i]])
        }
    }
}

\references{The NOAA website
    \url{http://www.ngdc.noaa.gov/mgg/shorelines/shorelines.html} may be a good
    starting point for a search for coastline data.  The ``shapefile'' format
    is described in \emph{ESRI Shapefile Technical Description}, March 1998,
    available at
    \url{http://www.esri.com/library/whitepapers/pdfs/shapefile.pdf}.}

\author{Dan Kelley}

\keyword{misc}

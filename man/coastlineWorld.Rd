\name{coastlineWorld}

\docType{data}

\alias{coastlineWorld}

\title{World coastline}

\description{World coastline}

\usage{data(coastlineWorld)}

\details{The longitudes are in the range from -180 to 180 degrees, i.e.
  western longitudes have negative values.  Created by downloading the
  file \code{TM_WORLD_BORDERS-0.3.zip} from 
  \url{http://thematicmapping.org/downloads/world_borders.php}, then
  unzipping and running the following:
  \preformatted{
    coastlineWorld <- read.coastline.shapefile("TM_WORLD_BORDERS-0.3.shp")
    save(coastlineWorld, file="coastlineWorld.rda", compress="xz")
  }
}

\source{\url{http://thematicmapping.org}, which provides the data in a
  zipfile containing the above-named \code{.shp} file, covered by a
  Creative Commons license
  (\url{http://creativecommons.org/licenses/by-sa/3.0/}).}

\seealso{Coastlines may be read with
\code{\link{read.coastline}}, or created with
\code{\link{as.coastline}}; see these for details of the data format.  Other coastline datasets provided with
oce are \code{\link{coastlineHalifax}} and \code{\link{coastlineMaritimes}}.}

\author{Dan Kelley}

\keyword{datasets}

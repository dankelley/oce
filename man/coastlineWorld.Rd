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

  Unfortunately, this dataset does not contain lakes, and so a search is on for
  a replacement.

}

\source{\url{http://thematicmapping.org}, which provides the data in a
  zipfile containing the above-named \code{.shp} file, covered by a
  Creative Commons license
  (\url{http://creativecommons.org/licenses/by-sa/3.0/}).}

\seealso{The documentation for \code{\link{coastline-class}} explains the
    structure of coastline objects, and also outlines the other functions
    dealing with them.}

\author{Dan Kelley}

\keyword{datasets}

\name{topo.maritimes}

\docType{data}

\alias{topo.maritimes}

\title{Sample topographic dataset, showing Maritime region of Canada}

\description{Topography of the Gulf of St Lawrence region, including
  portions of the Canadian provinces known as the Maritimes, as well
  as Newfoundland.}

\usage{data(topo.maritimes)}

\source{The dataset was downloaded in late 2008 from the NOAA website,
  with a longitude box from 70W to 55W and a latitude box from 43N to
  53N, using the ETOPO1 database, which has 1-minute resolution in
  latitude and longitude, and 0.1m resolution in the vertical.  The
  data were downloaded with an ascii header and ascii-raster format,
  so that \code{\link{read.topo}} could handle the resultant
  downloaded file without modification.}

\references{\url{http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html}}

\seealso{Files may be read with \code{\link{read.topo}},
  summarized with \code{\link{summary.topo}}, and plotted with
  \code{\link{plot.topo}}.}

\author{Dan Kelley}

\keyword{misc}

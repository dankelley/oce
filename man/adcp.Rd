\name{adcp}

\docType{data}

\alias{adcp}

\title{Sample ADCP dataset}

\description{This is a sampling of measurements made with an
  upward-pointing ADCP (acoustic doppler current profiler) manufactured
  by Teledyne-RDI, as part of the St Lawrence Internal Wave Experiment.}

\examples{
library(oce)
data(adcp)
adcp.earth <- adcp.frame2earth(adcp.beam2frame(adcp))

# Plot overview images
plot(adcp.earth, which=1)

# Plot diagnostic timeseries (e.g. heading should *not* vary)
plot(adcp.earth, which=13:18)
}

\usage{data(adcp)}

\source{This file was created with the following command, which uses
  a large file (a week of 10-second data) not provided with the package:
\preformatted{
adcp <- read.oce(file="~/SL08F001.000", as.POSIXct("2008-06-26",tz="GMT"), as.POSIXct("2008-06-27", tz="GMT"), "30:00")
}
}

\seealso{See \code{\link{read.adcp}} for notes on the contents of
  \code{adcp} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{misc}

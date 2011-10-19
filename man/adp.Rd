\name{adp}

\docType{data}

\alias{adp}

\title{ADP (acoustic-doppler profiler) dataset}

\description{This is a sampling of measurements made with an upward-pointing
    ADP (acoustic doppler current profiler) manufactured by Teledyne-RDI, as
    part of the St Lawrence Internal Wave Experiment (SLEIWEX).}

\examples{
\dontrun{
# How data were created
beam <- read.oce(
  "/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000",
  from=as.POSIXct("2008-06-26", tz="UTC"), 
  to=as.POSIXct("2008-06-27", tz="UTC"),
  by="10:00", 
  latitude=47.88126, longitude=-69.73433)
xyz <- beamToXyzAdp(beam)
adp <- xyzToEnuAdp(xyz, declination=-18.1)
}

library(oce)
data(adp)

# Velocity components.  (Note: we should probably trim some bins at top.)
plot(adp)

# Note that tides have moved the mooring.
plot(adp, which=15:18)

# Do currents line up with bathymetry?
par(mfrow=c(1,2))
plot(adp, which="uv+ellipse+arrow")
data(coastlineSLE)
# plot local area
plot(coastlineSLE, center=c(adp@metadata$latitude,adp@metadata$longitude),
  span=50)
points(adp@metadata$longitude, adp@metadata$latitude, cex=3, col="red")
}

\usage{data(adp)}

\source{This file came from the SLEIWEX-2008 experiment.}

\seealso{See \code{\link{read.adp}} for notes on the contents of
  \code{adp} objects, and for functions to work with them.  Also, see
  \code{\link{plot.adp}} to learn about plotting ADP objects.}

\author{Dan Kelley}

\keyword{datasets}

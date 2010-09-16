\name{adp}

\docType{data}

\alias{adp}

\title{ADP (acoustic-doppler profiler) dataset}

\description{This is a sampling of measurements made with an
  upward-pointing ADP (acoustic doppler current profiler) manufactured
  by Teledyne-RDI, as part of the St Lawrence Internal Wave Experiment.}

\examples{
\dontrun{
# How data were created
beam <- read.oce("/data/archive/sleiwex/2008/moorings/m09/adp/rdi_2615/raw/adp_rdi_2615.000",
                 from=as.POSIXct("2008-06-26", tz="UTC"), 
                 to=as.POSIXct("2008-06-27", tz="UTC"),
                 by="10:00", 
                 latitude=47.88126, longitude=-69.73433)
xyz <- adp.beam2xyz(beam)
adp <- adp.xyz2enu(xyz, declination=-18.1)
}

library(oce)
data(adp)

# images of velocity components
plot(adp)

# see if tides moved the mooring
plot(adp, which=15:18)

# horizontal velocity scattergram, and coastline
par(mfrow=c(1,2))
plot(adp, which=28.1)
data(coastline.sle)
plot(coastline.sle,xlim=c(-70,-69.5), ylim=c(47.7,48.1))
points(-69.73433, 47.88126, col="red")
}

\usage{data(adp)}

\source{This file came from the SLEIWEX-2008 experiment.}

\seealso{See \code{\link{read.adp}} for notes on the contents of
  \code{adp} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{misc}

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
plot(adcp.earth)

# Plot diagnostic timeseries.  Angles should be constant for a mooring.
plot(adcp.earth, which=13:18)
}

\usage{data(adcp)}

\source{This file was created with the following command (which uses
  a large file that is not provided with the package).  Note the
  use of UTC, to match how the instrument was set up.
\preformatted{
adcp <- read.oce("~/SL08F001.000", 
                 from=as.POSIXct("2008-06-26", tz="UTC"),
                 to=as.POSIXct("2008-06-27", tz="UTC"), 
                 by="30:00")
}
}

\seealso{See \code{\link{read.adcp}} for notes on the contents of
  \code{adcp} objects, and for functions to work with them.}

\author{Dan Kelley}

\keyword{misc}

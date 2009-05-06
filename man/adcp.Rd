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
adcp <- read.oce(file = "~/SL08F001.000", 
                 skip = 5040, read = 1 + 24 * 2, stride = 30 * 6)
}
}

\seealso{
\code{\link{read.adcp}} reads \code{adcp} objects,
\code{\link{summary.adcp}} summarizes them, and \code{\link{plot.adcp}} plots them.
\code{\link{adcp.beam.attenuate}} converts an \code{adcp} object from beam to velocity.
\code{\link{adcp.earth2other}} rotates \code{adcp} object currents from earth coordinates to other coordinates.
\code{\link{adcp.frame2earth}} converts an \code{adcp} object from frame coordinates to earth coordinates.
}


\author{Dan Kelley}

\keyword{misc}

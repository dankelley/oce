\name{adcp}

\docType{data}

\alias{adcp}

\title{Sample ADCP dataset}

\description{This is a 10 minute segment of data measured with an
  upward-pointing ADCP (acoustic doppler current profiler) manufactured
  by Teledyne-RDI.}

\examples{
library(oce)
data(adcp)
adcp.earth <- adcp.frame2earth(adcp.beam2frame(adcp))
plot(adcp.earth)
}

\usage{data(adcp)}

\source{\code{adcp <- read.oce(file = "~/SL08F001.000", skip = 5000, read = 60)}}

\seealso{See \code{\link{read.adcp}} for information on reading such
  files, the data stored therein, and the various functions provided to
  work with ADCP data.}

\author{Dan Kelley}

\keyword{misc}

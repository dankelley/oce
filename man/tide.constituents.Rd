\name{tide.constituents}
\docType{data}
\alias{tide.constituents}
\title{Tidal constituent data}
\description{
Tide-constituent data, based on the \code{tide3.dat} file provided
with the \code{t.tide} matlab package for tidal analysis (Pawlowicz et
al., 2002), which is in turn based on Foreman's (1977) manual for his
FORTRAN code for tidal analysis.

The dataset was created from the following code, using a file stored
in the tests portion of the package.

\preformatted{
# FIXME: should also be analysing the rest of this file!
create.constituents <- function(file="tide3.dat", nrows=146)
{
  x <- read.table(file=file, nrows=nrows, fill=TRUE, as.is=TRUE, header=FALSE)
  data.frame(name=x$V1, frequency=x$V2, compare=x$V3, standard=x$V3!="", stringsAsFactors=FALSE)
}
tide.constituents <- create.constituents()
save(tide.constituents, file="oce/data/tide.constituents.rda")
}
}
\examples{
library(oce)
data(tide.constituents)
cat("The so-called standard constituents are as follows:",
    tide.constituents$name[tide.constituents$standard])
}
\usage{data(tide.constituents)}
\source{See above.}
\references{
Rich Pawlowicz, Bob Beardsley, and Steve Lentz, 2002.
Classical tidal harmonic analysis including error estimates in
MATLAB using T TIDE.
Computers and Geosciences, 28, 929-937.

Foreman, M.G.G., 1978.
Manual for tidal currents analysis and prediction.
Pacific Marine Science Report 78-6, Institute of 
Ocean Sciences, Patricia Bay, Sidney, BC, 57pp.

Foreman, M.G.G., Neufeld, E.T., 1991.
Harmonic tidal analyses of long time series.
International Hydrographic Review, 68 (1), 95-108.}
\seealso{}
\author{Dan Kelley}
\keyword{misc}

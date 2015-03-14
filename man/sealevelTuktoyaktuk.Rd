\name{sealevelTuktoyaktuk}

\docType{data}

\alias{sealevelTuktoyaktuk}

\title{Sea-level data set acquired in 1975 at Tuktoyaktuk}

\description{This sea-level dataset is provided with in Appendix 7.2 of
  Foreman (1977) and also with the \code{T_TIDE} package (Pawlowicz et al.,
  2002). It results from measurements made in 1975 at Tuktoyaktuk,
  Northwest Territories, Canada.

  The data set contains 1584 points, some of which have NA for sea-level
  height.  
  
  Although Foreman's Appendix 7.2 states that times are in Mountain standard
  time, the timezone is set to \code{UTC} in the present case, so that the
  results will be similar to those he provides in his Appendix 7.3.  
}

\examples{
\dontrun{
library(oce)
data(sealevelTuktoyaktuk)
time <- sealevelTuktoyaktuk[["time"]]
elevation <- sealevelTuktoyaktuk[["elevation"]]
oce.plot.ts(time, elevation, type='l', ylab="Height [m]", ylim=c(-2,6))
legend("topleft", legend=c("Tuktoyaktuk (1975)","Detided"),
       col=c("black","red"),lwd=1)
tide <- tidem(sealevelTuktoyaktuk)
detided <- elevation - predict(tide)
lines(time, detided, col="red")
}
}

\usage{data(sealevelTuktoyaktuk)}

\source{The data were based on the \code{T_TIDE} dataset,  which in turn seems
    to be based on Appendix 7.2 of Foreman (1977).  Minor editing was on file
    format, and then the \code{sealevelTuktoyaktuk} object was created using
    \code{\link{as.sealevel}}.}

\references{
  Foreman, M. G. G., 1977.
  Manual for tidal heights analysis and prediction.
  Pacific Marine Science Report 77-10,
  Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 58pp.

  Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
  Classical tidal harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
  Computers and Geosciences, 28, 929-937.
}

\seealso{The documentation for \code{\link{sealevel-class}} explains the
    structure of \code{sealevel} objects, and also outlines the other functions
    dealing with them.  Data for Halifax harbour are stored in
    \code{\link{sealevel}}.}

\keyword{datasets}


\name{sealevel.tuk}

\docType{data}

\alias{sealevel.tuk}

\title{Sample sea-level data set, from Tuktoyaktuk (1975)}

\description{This sea-level dataset is provided with in Appendix 7.2 of
  Foreman (1977) and also with the \code{T_TIDE} package (Pawlowicz et al.,
  2002). It results from measurements made in 1975 at Tuktoyaktuk,
  Northwest Territories, Canada.

  The data set is a test contains 1584 points, some of which have NA for
  sea-level height.  The first point in the set is at time 1975-07-06
  0800 GMT.  However, the first 15 points of the series have NA for the
  sealevel value, so that the first non-missing point is at 2300 GMT, or
  1600 MST as in Foreman's Appendix 7.2.
} % description

\examples{
data(sealevel.tuk)
plot(sealevel.tuk$data$time, sealevel.tuk$data$elevation, type='l',
     ylab="Height [m]",ylim=c(-2,6))
legend("topright", legend=c("Tuktoyaktuk (1975)","Detided"),
       col=c("black","red"),lwd=1)
tide <- tidem(sealevel.tuk)
detided <- sealevel.tuk$data$elevation - predict(tide)
lines(sealevel.tuk$data$time, detided, col="red")
}

\usage{data(sealevel.tuk)}

\source{The data were extracted from the \code{T_TIDE} dataset (which
  Pawlowicz et al. (2002) seems to have based on Appendix 7.2 of Foreman
  (1977), to build a test case), edited a bit to get the dates in a
  format that \R could scan, and put into a data file.  This file was
  then used to create \code{sealevel} object follows.)

\preformatted{
# Note: the data file is not supplied with the package,
# and the 7h offset is to convert from Mountain Standard Time.
tuk <- read.table("tuk/tuk_time_elev.dat", header=FALSE, as.is=TRUE)
time <- as.POSIXlt(strptime(tuk$V1, "\%d-\%b-\%Y \%H:\%M:\%S", tz="GMT")+7*3600, tz="GMT")
eta <- tuk$V2
sealevel.tuk <- as.sealevel(eta=eta, time=time,
                            station.name="Tuktoyaktuk", region="NWT", station.number=6485,
                            longitude=133.0292, latitude=69.43889,
                            year=1975, GMT.offset=0)
#save(sealevel.tuk, file="oce/data/sealevel.tuk.rda")
} % preformatted
} % source

\references{
  Foreman, M. G. G., 1977.
  Manual for tidal heights analysis and prediction.
  Pacific Marine Science Report 77-10,
  Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 58pp.

  Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
  Classical tidal harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
  Computers and Geosciences, 28, 929-937.
}

\author{Dan Kelley, after data provided in Foreman's (1977) Appendix 7.2.}

\keyword{misc}

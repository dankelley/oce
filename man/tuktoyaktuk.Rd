\name{tuktoyaktuk}
\docType{data}
\alias{tuktoyaktuk}
\title{Sample sea-level data set, from Tuktoyaktuk (1975)}


\description{This sea-level dataset is provided with in Appendix 7.2 of Foreman
(1977) and also with the T\_TIDE package (Pawlowicz et al., 2002). It results from
measurements made in 1975 at Tuktoyaktuk, Northwest Territories, Canada.

The data set is a test contains 1584 points, some of which have NA for sea-level
height.  The first point is at time 1975-07-06 23:00:00 GMT (16:00 MST,
as given by Foreman).  The first 15 points have missing values for sea
level.
} % description

\section{Example}{
\preformatted{
data(tuktoyaktuk)
plot(tuktoyaktuk$data$t, tuktoyaktuk$data$eta, type='l',
    ylab="Height [m]",ylim=c(-2,6))
legend("topright", legend=c("Tuktoyaktuk (1975)","Detided"),
    col=c("black","red"),lwd=1)
tide <- fit.tide(tuktoyaktuk)
detided <- tuktoyaktuk$data$eta - predict(tide$model)
lines(tuktoyaktuk$data$t, detided, col="red")
} % preformatted
} % example

\source{The data were extracted from the \code{T_TIDE} dataset (which 
Pawlowicz et al. (2002) seems to have based on
Appendix 7.2 of Foreman (1977), to build a test case),
edited a bit to get the dates in a format that \R could
scan, and put into a data file.  This file was then used to create
\code{sealevel} object follows.  (The \code{T\_TIDE} dataset started at
\code{06-Jul-1975 01:00:00}, but the correct GMT start time
is \code{06-Jul-1975 23:00:00}.)
\preformatted{
# NB. the data file is not supplied with the package
tuk <- read.table("tuk/tuk_time_elev.dat", header=FALSE, as.is=TRUE)
t <- as.POSIXlt(strptime(tuk$V1, "\%d-\%b-\%Y \%H:\%M:\%S", tz="GMT")+22*3600, tz="GMT")
eta <- tuk$V2
tuktoyaktuk <- as.sealevel(eta=eta, t=t)
#save(tuktoyaktuk, file="oce/data/tuktoyaktuk.rda")
} % preformatted
} % source

\references{
Foreman, M. G. G., 1977.
Manual for tidal heights analysis and prediction.
Pacific Marine Science Report 77-10,
Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 58pp.

Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
Classical tidal harmonic analysis including error estimates in MATLAB using T\_TIDE.
Computers and Geosciences, 28, 929-937.
}
\author{Dan Kelley, after data provided in Foreman's (1977) Appendix 7.2.}
\keyword{misc}

\name{tuktoyaktuk}
\docType{data}
\alias{tuktoyaktuk}
\title{Sample sea-level data set, from Tuktoyaktuk (1975)}


\description{This sample sea-level dataset is the one provided with the Foreman (1977)
and Pawlowicz, from Tuktoyaktuk.  It contains 1584 points, a fraction of which have NA for 
sea-level height, eta.


\strong{Test of fit.tide():} The Foreman package produces M2 amplitude 0.4904m and phase
77.50 (Appendix 7.3), when P1 and K2 are added to the standard constituents. Under the same
conditions, or at any rate for its demonstration, T\_TIDE gives 0.4904m \eqn{\pm}{+/-}
0.037m and phase 77.70 \eqn{\pm}{+/-} 3.94. \strong{However}, fit.tide produces
0.500723648m and phase.


An example of using this is as follows.

\preformatted{
data(tuktoyaktuk)
par(mfrow=c(2,1))
par(mar=c(3,4,3,2))
plot(tuktoyaktuk$data$t, tuktoyaktuk$data$eta, type='l', ylab="Height [m]")
legend("topright", legend="Tuktoyaktuk sea level (1975)")
tide <- fit.tide(tuktoyaktuk)
detided <- tuktoyaktuk$data$eta - predict(tide$model)
plot(tuktoyaktuk$data$t, detided, type='l',ylab="Height [m]")
legend("topright", legend="After removal of tide")
} % preformatted

} % description

\source{The data were extracted from the T\_TIDE set, put into a data file, and then built into a
\code{sealevel} object with the following code.
\preformatted{
tuk <- read.table("tuk_time_elev.dat",header=FALSE)
# MST is GMT minus 7 hours
t   <- as.POSIXlt(strptime(tuk$V1, "%d-%b-%Y %H:%M:%S")) - 7 * 3600
eta <- tuk$V2
tuktoyaktuk <- as.sealevel(eta=eta, t=t)
} % preformatted
} % source

\references{
Foreman, M. G. G., 1978.
Manual for tidal currents analysis and prediction. Pacific Marine Science Report 78-6, 
Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 57pp.

Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
Classical tidal harmonic analysis including error estimates in MATLAB using T\_TIDE.
Computers and Geosciences, 28, 929-937.
}
\author{Dan Kelley}
\keyword{misc}

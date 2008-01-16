\name{tuktoyaktuk}
\docType{data}
\alias{tuktoyaktuk}
\title{Sample sea-level data set, from Tuktoyaktuk (1975)}


\description{This sea-level dataset is provided with in Appendix 7.2 of Foreman
(1977) and also with the T\_TIDE package (Pawlowicz et al., 2002). It results from
measurements made in 1975 at Tuktoyaktuk, Northwest Territories, Canada.

The data set is a test contains 1584 points, some of which have NA for sea-level
height.
} % description

\section{Example}{
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
} % example

\section{Test of fit.tide()}{

\strong{Note: this is only a preliminary test, since \code{\link{fit.tide}} does
not yet do nodal corrections, and also since the test dos not add the P1 and K2
constituents, as in the Foreman and T\_TIDE cases.
FIXME: delete this note when the code is finished.}

The Foreman package produces M2 amplitude 0.4904m and phase 77.50 (Appendix 7.3),
when P1 and K2 are added to the standard constituents.

\strong{FIXME: add uncertainties from Foreman.}

Under what one may assume are the same conditions, 
the demonstration withing T\_TIDE gives
0.4904m \eqn{\pm}{+/-} 0.037m and phase 77.70 \eqn{\pm}{+/-} 3.94.

Under the same conditions, \code{\link{fit.tide}}
produces 0.5007m and phase 112.22.

\strong{FIXME: fix the above when the code is completed.}

} % section

\source{The data were extracted from the T\_TIDE dataset (which 
Pawlowicz et al. (2002) copied from
Appendix 7.2 of Foreman (1977), to build a test case),
edited a bit to get the dates in a format that \R could
scan, and put into a data file.  This file was then used to create
\code{sealevel} object follows.
\preformatted{
tuk <- read.table("tuk_time_elev.dat",header=FALSE)
offset <- 7 * 60 * 60 # MST, in the file, is GMT minus 7 hours
t   <- as.POSIXlt(strptime(tuk$V1, "%d-%b-%Y %H:%M:%S")) + offset
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

\name{tuktoyaktuk}
\docType{data}
\alias{tuktoyaktuk}
\title{Sample sea-level data set, from Tuktoyaktuk}
\description{This sample sea-level dataset is the one provided with the Foreman (1977)
and Pawlowicz, from Tuktoyaktuk}

\source{The data were extracted from the T_TIDE set, put into a data file, and then built into a
\code{sealevel} object with the following code.
\preformatted{
tuk <- read.table("tuk_time_elev.dat",header=FALSE)
tuktoyaktuk <- as.sealevel(eta=tuk$V2, t=strptime(tuk$V1, "%d-%b-%Y %H:%M:%S"))
save(tuktoyaktuk, file="~/src/R-kelley/oce/data/tuktoyaktuk.rda")
}
}

\references{
Foreman, M. G. G., 1978.
Manual for tidal currents analysis and prediction. Pacific Marine Science Report 78-6, 
Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 57pp.

Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
Classical tidal harmonic analysis including error estimates in MATLAB using T_TIDE.
Computers and Geosciences, 28, 929-937.
}
\author{Dan Kelley}
\keyword{misc}

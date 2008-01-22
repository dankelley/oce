\name{tidem}
\alias{tidem}
\title{Fit a tidal model to a timeseries}
\description{Fit a tidal model to a timeseries.}
\usage{tidem(sl, constituents, rc=1)}
\arguments{
  \item{sl}{a \code{sealevel} object.}
  \item{constituents}{an optional list of tidal constituents to which
	the fit is done (see \dQuote{Details}.)}  
  \item{rc}{the value of the coefficient in the Rayleigh criterion.}
}

\details{The fit is done in terms of sine and cosine components at the
  indicated tidal frequencies, with the amplitude and phase being
  calculated from the resultant coefficients on the sine and cosine
  terms.
  
  The tidal constituents to be used in the analysis are specified as
  follows.

  \enumerate{

  \item{Case 1.}{If \code{constituents} is not provided, then the
  constituent list will be made up of the 69 constituents regarded by
  Foreman as standard.  These include astronomical frequencies and some
  shallow-water frequencies, and are as follows: \code{c("Z0", "SA",
  "SSA", "MSM", "MM", "MSF", "MF", "ALP1", "2Q1", "SIG1", "Q1", "RHO1",
  "O1", "TAU1", "BET1", "NO1", "CHI1", "PI1", "P1", "S1", "K1", "PSI1",
  "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2", "2N2",
  "MU2", "N2", "NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2",
  "T2", "S2", "R2", "K2", "MSN2", "ETA2", "MO3", "M3", "SO3", "MK3",
  "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4", "2MK5", "2SK5",
  "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")}.}

  \item{Case 2.}{If the first item in \code{constituents} is the string
  \code{"standard"}, then a provisional list is set up as in Case 1, and
  then the (optional) rest of the elements of \code{constituents} are
  examined, in order.  Each of these constituents is based on the name
  of a tidal constituent in the Foreman (1977) notation.  (To get the
  list, do \code{data(tide.constituents);cat(tide.constituents$name)}.)
  Each named constituent is added to the existing list, if it is not
  already there.  But, if the constituent is preceeded by a minus sign,
  then it is removed from the list (if it is already there).  Thus, for
  example, \code{constituents=c("standard", "-M2", "ST32")} would remove
  the M2 constituent and add the ST32 constituent.}

  \item{Case 3.}{If the first item is not \code{"standard"}, then the
  list of constituents is processed as in Case 2, but without starting
  with the standard list. As an example, \code{constituents=c("K1",
  "M2")} would fit for just the K1 and M2 components. (It would be
  strange to use a minus sign to remove items from the list, but the
  function allows that.)}

  In each of the above cases, the list is reordered in frequency prior
  to the analysis, so that the results of \code{\link{summary.tide}}
  will be in a familiar form.

  Once the constituent list is determined, \code{tidem} prunes the
  elements of the list by using the Rayleigh criterion, according to
  which two constituents of frequencies \eqn{f_1}{f1} and \eqn{f_2}{f2}
  cannot be resolved unless the time series spans a time interval of at
  least \eqn{rc/(f_1-f_2)}{rc/(f1-f2)}. The value \code{rc=1} yields
  nominal resolution.

  \strong{The text should include discussion of the (not yet performed)
  nodal correction treatement.}
} % enumerate
} % details

\section{Bugs}{
  
  \enumerate{
    \item{1.}{This function is not fully developed yet, and both the
      form of the call and the results of the calculation may change.}
    
    \item{2.}{Nodal correction is not done.}
    
    \item{3.}{The reported \code{p} value may make no sense at all, and it might be removed in a
      future version of this function. Perhaps a significance level should be presented, as in the
      software developed by both Foreman and Pawlowicz.}
    
    \item{4.}{The phase is wrong, at least judging by the Foreman
      test data set. (For M2, this gives -98.838841 but Foreman gives 77.70.)}
    
  } % enumerate
  
} % note


\value{An object of \code{\link[base]{class}} \code{"tide"}, consisting of
  \item{model}{the regression model}
  \item{name}{a vector of constituent names, in non-subscript format, e.g. "\code{M2}".}
  \item{frequency}{a vector of constituent frequencies, in inverse hours.}
  \item{amplitude}{a vector of fitted constituent amplitudes, in metres.}
  \item{phase}{a vector of fitted constituent phase.  (FIXME: this needs to be resolved to Greenwich, and it is not, yet.)}
  \item{p}{a vector containing a sort of p value for each constituent.  This is calculated as the average of the p values for the sine() and cosine() portions used in fitting; whether it makes any sense is an open question.}
}

\section{Test with data(sealevel.tuk)}{

\strong{Note: this is only a preliminary test, since
\code{\link{tidem}} does not yet do nodal corrections, and also since
the test dos not add the P1 and K2 constituents, as in the Foreman and
T\_TIDE cases.  FIXME: delete this note when the code is finished.}

The Foreman package produces M2 amplitude 0.4904m and phase 77.70
(Appendix 7.3), when P1 and K2 are added to the standard constituents.
Under what one may assume are the same conditions, the demonstration
within \code{T\_TIDE} gives 0.4904m \eqn{\pm}{+/-} 0.037m and phase
77.70 \eqn{\pm}{+/-} 3.94.

Under the same conditions, the test given below results in 
amplitude 0.5007m and phase -98.838841.  
\preformatted{
  data(sealevel.tuk)
  tuk.tide <- tidem(sealevel.tuk)
  summary(tuk.tide)
}

} % section

\seealso{\code{\link{summary.tide}} summarizes a "\code{tide}" object,
  and \code{\link{plot.tide}} plots one.  As for the input, sealevel
  objects may be created with \code{\link{as.sealevel}} or
  \code{\link{read.sealevel}}.  See notes at \code{\link{sealevel.tuk}},
  which is test data set.
}

\examples{
library(oce)
# Fake a M2 tide with phase 45 degrees
hour <- seq(0, 24 * 100)
eta <- 1 + 2 * sin(0.08051140 * 2 * pi * hour + pi/4)
sealevel <- as.sealevel(eta)
tide <- tidem(sealevel)
summary(tide)
plot(tide)

# The demonstration time series from Foreman (1977),
# also used in T_TIDE (Pawlowicz, 2002).
data(sealevel.tuk)
tuk.tide <- tidem(sealevel.tuk)
summary(tuk.tide)
}
\references{
  Foreman, M. G. G., 1977.
  Manual for tidal heights analysis and prediction.
  Pacific Marine Science Report 77-10,
  Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 58pp.
  
  Foreman, M. G. G., Neufeld, E. T., 1991.
  Harmonic tidal analyses of long time series.
  International Hydrographic Review, 68 (1), 95-108.
  
  Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
  Classical tidal harmonic analysis including error estimates in MATLAB using T\_TIDE.
  Computers and Geosciences, 28, 929-937.
}
\author{Dan Kelley}
\keyword{misc}

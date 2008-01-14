\name{fit.tide}
\alias{fit.tide}
\title{Fit a tidal model to a timeseries}
\description{Fit a tidal model to a timeseries.}
\usage{fit.tide(sl, constituents="standard", rc=1)}
\arguments{
  \item{sl}{a \code{sealevel} object.}
  \item{constituents}{either "\code{standard}" for 69 commonly-used constituents, or a list of constituent names (see \sQuote{Details}).}
  \item{rc}{the value of the coefficient in the Rayleigh criterion.}
}

\details{The fit is done in terms of sine and cosine components at the indicated tidal
frequencies, with the amplitude and phase being calculated from the resultant coefficients
on the sine and cosine terms.

If \code{constituents} is set to "\code{standard}", then the 69 constituents regarded by
Foreman as standard are used. These include astronomical frequencies and some shallow-water
frequencies, and are as follows: \code{c("Z0", "SA", "SSA", "MSM", "MM", "MSF", "MF",
"ALP1", "2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1", "NO1", "CHI1", "PI1", "P1", "S1",
"K1", "PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2", "2N2", "MU2", "N2",
"NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2",
"ETA2", "MO3", "M3", "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4",
"2MK5", "2SK5", "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")}.

If \code{constituents} is given as a list, then just the named constituents are used, e.g.
to fit just M2 and K1, use \code{c("K1", "M2")}. The constituents should be provided in
order of increasing frequency. For a list of constituent names and their frequencies, see
\code{\link{tide.constituents}}.

The Rayleigh criterion -- which is \strong{ignored} in the present version of the package --
determines whether the data set is long enough to distinguish between constituents with
similar frequencies. The basic idea is simple: constituents of frequencies
\eqn{\omega_1}{o1} and \eqn{\omega_2}{o2} cannot be resolved unless the time series spans a
time interval of at least \eqn{rc/(\omega_1-\omega_2)}{rc/(o1-o2)}. The value \code{rc=1} is
sufficient for nominal resolution, but a cautious analyst might use a larger value. This is
simple enough, but it leaves open the practical question of what to do if the timeseries is
too short to resolve the constituents that were initially of interest (perhaps even the full
list of 146 constituents provided in this package). Foreman (1977) addresses this question
in detail in his section 2.1.2, to which the reader is directed for general education, and
to which the author will be paying attention as he further develops this package.

}

\note{
This function is not fully developed yet, and both the form of the call and the results of the calculation may very well change.  

There are also some bugs:

\enumerate{
\item{1.}{The phase is not resolved to Greenwich.}

\item{2.}{The Rayleigh criterion is not used. Instead, the fit is done to all specified
constituents, even if the criterion would suggest that energy might be blurred between some
of the constituents.}

\item{3.}{Nodal correction is not done.}

\item{4.}{The reported \code{p} value may make no sense at all, and it might be removed.  Perhaps 
a significance level should be presented, as in the software developed by both Foreman and Pawlowicz.}
} % enumerate
} % note


\value{An object of class \dQuote{\code{tide}}, consisting of
\item{model}{the regression model}
\item{name}{a vector of constituent names, in non-subscript format, e.g. \dQuote{\code{M2}}.}
\item{frequency}{a vector of constituent frequencies, in inverse hours.}
\item{amplitude}{a vector of fitted constituent amplitudes, in metres.}
\item{phase}{a vector of fitted constituent phase.  (FIXME: this needs to be resolved to Greenwich, and it is not, yet.)}
\item{p}{a vector containing a sort of p value for each constituent.  This is calculated as the average of the p values for the sine() and cosine() portions used in fitting; whether it makes any sense is an open question.}
}

\seealso{\code{\link{tide.constituents}} provides the frequencies of the tidal constituents. \code{\link{summary.tide}} summarizes a \dQuote{\code{tide}} object, and \code{\link{plot.tide}} plots one.}

\examples{
library(oce)
# Fake an M2 tide, to check
hour <- seq(1, 24 * 365)
eta <- 1 + sin(0.08051140 * 2 * pi * hour)
sealevel <- as.sealevel(eta)
tide <- fit.tide(sealevel)
summary(tide)
plot(tide)
}
\references{
Rich Pawlowicz, Bob Beardsley, and Steve Lentz, 2002. Classical tidal harmonic analysis including error estimates in MATLAB using T TIDE. Computers and Geosciences, 28, 929-937.

Foreman, M.G.G., 1978. Manual for tidal currents analysis and prediction. Pacific Marine Science Report 78-6, Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 57pp.

Foreman, M.G.G., Neufeld, E.T., 1991. Harmonic tidal analyses of long time series. International Hydrographic Review, 68 (1), 95-108.
}
\author{Dan Kelley}
\keyword{misc}

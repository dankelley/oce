\name{fit.tide}
\alias{fit.tide}
\title{Fit a tidal model to a timeseries}
\description{Fit a tidal model to a timeseries.}
\usage{fit.tide(sl, constituents="standard")}
\arguments{
  \item{sl}{a \code{sealevel} object.}
  \item{constituents}{either "\code{standard}" (see \sQuote{Details}) or a list of constituent names, e.g. \code{c("M2", "K1")}.}
}

\details{The fit is done in terms of sine and cosine components at the indicated tidal
frequencies, with the amplitude and phase being calculated from the resultant coefficients
on the sine and cosine terms.

If \code{constituents} is set to "\code{standard}", then the 69 constituents regarded by
Foreman as standard are used. These include astronomical frequencies and some shallow-water
frequencies, and are as follows: \code{c("Z0","SA", "SSA", "MSM", "MM", "MSF", "MF", "ALP1",
"2Q1", "SIG1", "Q1", "RHO1", "O1", "TAU1", "BET1", "NO1", "CHI1", "PI1", "P1", "S1", "K1",
"PSI1", "PHI1", "THE1", "J1", "SO1", "OO1", "UPS1", "OQ2", "EPS2", "2N2", "MU2", "N2",
"NU2", "GAM2", "H1", "M2", "H2", "MKS2", "LDA2", "L2", "T2", "S2", "R2", "K2", "MSN2",
"ETA2", "MO3", "M3", "SO3", "MK3", "SK3", "MN4", "M4", "SN4", "MS4", "MK4", "S4", "SK4",
"2MK5", "2SK5", "2MN6", "M6", "2MS6", "2MK6", "2SM6", "MSK6", "3MK7", "M8")}.

If \code{constituents} is given as a list, then just the named constituents are used (see
\sQuote{Examples}.  For a list of constituent names, see \code{\link{tide.constituents}}.)

This function can be somewhat slow for large data sets.

BUG: the phase is not resolved to Greenwich.

BUG: the Rayleigh criterion is not considered.

BUG: the reported \code{p} value may make no sense at all.

}

\value{An object of class \dQuote{\code{tide}}, consisting of
\item{model}{the regression model}
\item{name}{a vector of constituent names, in non-subscript format, e.g. \dQuote{\code{M2}}.}
\item{frequency}{a vector of constituent frequencies, in inverse hours.}
\item{amplitude}{a vector of fitted constituent amplitudes, in metres.}
\item{phase}{a vector of fitted constituent phase.  (FIXME: this needs to be resolved to Greenwich, and it is not, yet.)}
\item{p}{a vector containing a sort of p value for each constituent.  This is calculated as the average of the p values for the sine() and cosine() portions used in fitting; whether it makes any sense is an open question.}
}

\seealso{\code{\link{summary.tide}} summarizes a \dQuote{\code{tide}} object, and \code{\link{plot.tide}} plots one.}

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

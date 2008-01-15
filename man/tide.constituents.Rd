\name{tide.constituents}
\alias{tide.constituents}
\title{Tidal constituents}
\description{Tidal constituents, after Foreman (1977).}
\usage{tc <- tide.constituents()}
\arguments{
}
\details{Values are from Foreman (1977), also provided by the file \code{tide3.dat} in 
the t-tide package (Pawlowicz et al., 2002).}

\value{A list containing
\item{name}{a string name indicating the constituent name, in the notation of Foreman (1977).}
\item{frequency}{the frequency of the constituent, in cycles per hour.}
\item{compare}{a string indicating another constituent with which this should be compared in a Rayleigh-criterion calculation (see \code{\link{fit.tide}}).  The string is empty for 
\dQuote{standard} constituents (see next item).}
\item{standard}{a boolean indicating whether this constituent is \dQuote{standard}, in
the sense defined by Foreman (1977).}
}

\seealso{\code{\link{fit.tide}} uses these constituents to do a tidal decomposition analysis.}

\examples{
library(oce)
tc <- tide.constituents()
cat("M2 period:", 1 / tc$frequency[which(tc$name=="M2")], "h.\n")
cat("Standard tidal constituents:", tc$name[tc$standard])
}
\references{
Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002. Classical tidal harmonic analysis including error estimates in MATLAB using T\_TIDE. Computers and Geosciences, 28, 929-937.

Foreman, M. G. G., 1978. Manual for tidal currents analysis and prediction. Pacific Marine Science Report 78-6, Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 57pp.
}
\author{Dan Kelley}
\keyword{misc}

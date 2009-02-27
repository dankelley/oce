\name{summary.tide}

\alias{summary.tide}

\title{Summarize a tide object}

\description{Summarize a tide object.}

\usage{\method{summary}{tide}(object, p, constituent, \dots)}

\arguments{
  \item{object}{a \code{tide} object, as created by \code{\link{tidem}}.}
  \item{p}{optional value of the maximum p value for the display of an
    individual coefficient.  If not given, all coefficients are shown.}
  \item{constituent}{optional name of constituent on which to focus.}
  \item{\dots}{passed to children.}
}

\details{Pertinent summary information is presented.}

\value{An object of \code{\link[base]{class}} \code{"summary.tide"},
which is a data frame containing the following.
\item{Name}{the name of the constituent in non-subscript form, e.g. "\code{M2}".}
\item{Frequency}{the frequency of the constituent, in inverse hours.}
\item{Amplitude}{the amplitude of the constituent, in metres (see \code{\link{tidem}}).}
\item{Phase}{the phase of the constituent (see \code{\link{tidem}}).}
\item{p}{a sort of p value for the constituent (see \code{\link{tidem}}).}
\item{sig}{significance codes based on \code{p}, with symbols as in \code{\link{summary.lm}}.}
}

\seealso{\code{\link{tidem}} fits a tidal model to data (and describes
  the nature of \code{tide} objects), and \code{\link{plot.tide}} plots
  a \code{tide} object.}

\examples{
library(oce)
# Halifax data
data(sealevel.hal)
tide <- tidem(sealevel.hal)
summary(tide)
}

\author{Dan Kelley}

\keyword{misc}

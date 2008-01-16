\name{summary.tide}
\alias{summary.tide}
\title{Summarize a tide object}
\description{Summarize a tide object.}
\usage{
\method{summary}{tide}(object, p=0, \dots)
}
\arguments{
  \item{object}{a \code{tide} object, as created by \code{\link{fit.tide}}.}
  \item{p}{the minimum p value for the display of an individual coefficient.  The default causes all coefficients to be shown.}
  \item{\dots}{passed to children.}
}
\details{
  Pertinent summary information is presented.
}
\value{A \code{summary.tide} object is returned, which is a data frame containing
the following.
\item{Name}{the name of the constituent in non-subscript form, e.g. "\code{M2}".}
\item{Frequency}{the frequency of the constituent, in inverse hours.}
\item{Amplitude}{the amplitude of the constituent, in metres (see \code{\link{fit.tide}}).}
\item{Phase}{the phase of the constituent (see \code{\link{fit.tide}}).}
\item{p}{a sort of p value for the constituent (see \code{\link{fit.tide}}).}
\item{sig}{significance codes based on \code{p}, with symbols as in \code{\link{summary.lm}}.}
}
\seealso{
\code{\link{fit.tide}} fits a tidal model to data (and describes the nature of
\code{tide} objects), and \code{\link{plot.tide}} plots a \code{tide} object.
}
\examples{
library(oce)
data(sealevel)
tide <- fit.tide(sealevel)
summary(tide)
}
\author{Dan Kelley}
\keyword{misc}

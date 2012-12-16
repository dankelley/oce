\name{summary.tidem}

\alias{summary.tidem}
\alias{print.summary.tidem}

\title{Summarize a tide-model object}

\usage{\method{summary}{tidem}(object, p, constituent, \dots)}

\arguments{
  \item{object}{an object of class \code{"tidem"}, usually, a result of a
    call to \code{tidem}.}
  \item{p}{optional value of the maximum p value for the display of an
    individual coefficient.  If not given, all coefficients are shown.}
  \item{constituent}{optional name of constituent on which to focus.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\description{\code{summary} method for class \code{"tidem"}.}

\details{By default, all fitted constituents are plotted, but it is
  quite useful to set e.g. p=0.05 To see just those constituents that
  are significant at the 5 percent level.

  Note that the p values are estimated as the average of the p values
  for the sine and cosine components at a given frequency.}

\value{An object of class \code{"summary.tidem"}, which contains
  pertinent information about the fitted tidal model and its misfit to
  the data.}

\seealso{\code{\link{tidem}} fits a tidal model to data (and describes
  the nature of \code{tide} objects), and \code{\link{plot.tidem}} plots
  a \code{tide} object.}

\examples{
\dontrun{
library(oce)
data(sealevelHalifax)
tide <- tidem(sealevelHalifax)
summary(tide)
}
}

\author{Dan Kelley}

\keyword{misc}

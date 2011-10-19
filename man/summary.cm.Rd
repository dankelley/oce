\name{summary.cm}

\alias{summary.cm}

\title{Summarize a cm (current meter) object}

\description{Summarize a \code{cm} (current meter) object}

\usage{ \method{summary}{cm}(object, \dots) }

\arguments{
  \item{object}{an object of class \code{"cm"}, as read by
  \code{\link{read.cm.s4}}, perhaps called automatically by
  \code{\link{read.cm}} or \code{\link{read.oce}}.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{\code{summary.cm} returns an object of class \code{"summary.cm"},
  containing pertinent information about the \code{cm} object and its
  processing.}

\seealso{A \code{cm} object may  be read with \code{\link{read.cm}} or
  \code{\link{read.oce}}, and plotted with \code{\link{plot.cm}}.}

\examples{
\dontrun{
library(oce)
data(cm)
cm <- read.oce("cm_interocean_0811786.s4a.tab")
summary(cm)
}
}

\author{Dan Kelley}

\keyword{misc}

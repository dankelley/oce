\name{decimate}
\alias{decimate}
\title{Decimate an oce object}
\description{Decimate, or subsample, an oce object.}
\usage{decimate(x, by=10, method=c("direct"))}
\arguments{
  \item{x}{an \code{oce} object containing a \code{data} element.}
  \item{by}{an indication of the subsampling.  The first element is
    selected, then the \code{1+by} element, etc.}
  \item{method}{only one is provided at the moment: \code{direct}, which
    indicates straight decimate, or subsampling.}
}

\details{Later on, other methods will be added, and
  \code{\link{ctd.decimate}} will be retired in favour of this, a more
  general, function.}

\value{An object of \code{\link[base]{class}} \code{"oce"} that has been
  subsampled appropriately.}

\seealso{\code{ctd.decimate}, which will be retired when the present
  function gains equivalent functionality.}

\examples{
library(oce)
data(ctd)
d <- decimate(ctd, by=5)
plot(d)
}
\author{Dan Kelley}
\keyword{misc}

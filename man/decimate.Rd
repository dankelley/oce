\name{decimate}
\alias{decimate}
\title{Decimate an oce object}
\description{Decimate, or subsample, an oce object.}
\usage{decimate(x, by=10, to, filter, debug=getOption("oce.debug"))}
\arguments{
  \item{x}{an \code{oce} object containing a \code{data} element.}
  \item{by}{an indication of the subsampling.  The first element is
    selected, then the \code{1+by} element, etc.  This is ignored
    if \code{to} is given.}
  \item{to}{Indices at which to subsample.  If given, this over-rides
    \code{by}.}
  \item{filter}{optional list of numbers representing a digital filter
    to be applied to each variable in \code{x$data} before decimation is
    done. If not supplied, then the decimation is done strictly by
    sub-sampling.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
}

\details{Later on, other methods will be added, and
  \code{\link{ctd.decimate}} will be retired in favour of this, a more
  general, function.  The filtering is done with the
  \code{\link{filter}} function of the stats package.}

\value{An object of \code{\link[base]{class}} \code{"oce"} that has been
  subsampled appropriately.}

\seealso{Filter coefficients may be calculated using
  \code{\link{make.filter}}.  (Note that \code{ctd.decimate} will be
  retired when the present function gains equivalent functionality.)}

\examples{
library(oce)
data(adp)
plot(adp)
adp.dec <- decimate(adp,by=2,filter=c(1/4,1/2,1/4))
plot(adp.dec)
}
\author{Dan Kelley}
\keyword{misc}

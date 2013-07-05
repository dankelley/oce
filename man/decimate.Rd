\name{decimate}
\alias{decimate}
\title{Smooth and decimate an oce object}
\description{Smooth and decimate, or subsample, an oce object.}
\usage{decimate(x, by=10, to, filter, debug=getOption("oceDebug"))}

\arguments{

  \item{x}{an \code{oce} object containing a \code{data} element.}

  \item{by}{an indication of the subsampling.  If this is a single number, then
      it indicates the spacing between elements of \code{x} that are selected.
      If it is two numbers (a condition only applicabile if \code{x} is an
      \code{echosounder} object, at present), then the first number indicates
      the time spacing and the second indicates the depth spacing.}
      
  \item{to}{Indices at which to subsample.  If given, this over-rides
      \code{by}.} \item{filter}{optional list of numbers representing a digital
      filter to be applied to each variable in the \code{data} slot of
      \code{x}, before decimation is done. If not supplied, then the decimation
      is done strictly by sub-sampling.} \item{debug}{a flag that turns on
      debugging.  Set to 1 to get a moderate amount of debugging information,
      or to 2 to get more.}

}

\details{Later on, other methods will be added, and
  \code{\link{ctdDecimate}} will be retired in favour of this, a more
  general, function.  The filtering is done with the
  \code{\link{filter}} function of the stats package.}

\section{Bugs}{Only a preliminary version of this function is provided in the
    present package.  It only works for objects of class \code{echosounder},
    for which the decmation is done after applying a running median filter and
    then a boxcar filter, each of length equal to the corresponding component
    of \code{by}.}


\value{An object of \code{\link[base]{class}} \code{"oce"} that has been
  subsampled appropriately.}

\seealso{Filter coefficients may be calculated using
  \code{\link{makeFilter}}.  (Note that \code{\link{ctdDecimate}} will be
  retired when the present function gains equivalent functionality.)}

\examples{
library(oce)
data(adp, package="ocedata")
plot(adp)
adp.dec <- decimate(adp,by=2,filter=c(1/4,1/2,1/4))
plot(adp.dec)
}
\author{Dan Kelley}
\keyword{misc}

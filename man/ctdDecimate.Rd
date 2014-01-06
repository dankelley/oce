\name{ctdDecimate}
\alias{ctdDecimate}
\title{Decimate a CTD profile}
\description{Interpolate a CTD profile to specified pressure values.}
\usage{ctdDecimate(x, p, method=c("approx","boxcar","lm","rr", "unesco"),
             e=1.5, debug=getOption("oceDebug"))}
\arguments{
  \item{x}{a \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{p}{an optional pressure increment, or list of pressures. If
    \code{p} is not supplied, a list of regularly-spaced pressures is
    computed based on the mean pressure difference between samples
    (converted to a round value using \code{\link{pretty}}). This list
    goes from 0 to the maximum pressure in the profile (rounded to the
    pressure increment). If a single \code{p} value is specified, then
    it is used as the increment. Finally, if a list of pressures is
    provided, then interpolation is done to these pressures.}
  \item{method}{the method to be used for calculating decimated
    values. The default \code{"approx"} method is based on the R
    function \code{\link{approx}} and is a simple linear interpolation
    between neighbouring points. The \code{"boxcar"} method is based on
    the mean of local data , while the \code{"lm"} method is based on a
    local linear regression model (with \code{e} setting the
    locality, as explained below.)  The
    \code{"unesco"} and \code{"rr"} methods use \code{\link{oceApprox}}.}
  \item{e}{is an expansion coefficient used to calculate the local
    neighbourhoods for the \code{"boxcar"} and \code{"lm"} methods.  If
    \code{e=1}, then the neighbourhood for the i-th pressure extends from
    the (\code{i-1})-th pressure to the (\code{i+1})-th pressure.  At the
    endpoints it is assumed that the outside bin is of the same pressure
    range as the first inside bin.  For other values of \code{e}, the
    neighbourhood is expanded linearly in each direction.  If the
    \code{"lm"} method produces warnings about "prediction from a
    rank-deficient fit", a larger value of \code{"e"} should be used.}
  \item{debug}{a Boolean, set to \code{TRUE} to debug the reading process.}
}

\details{The \code{"approx"} method is best for bottle data, in which
  the usual task is to interpolate from a coarse sampling grid to a
  finer one. For CTD data, the \code{"boxcar"} method is probably the
  best choice, because the task is normally to sub-sample, and some
  degree of smoothing is usually desired. (The \code{"lm"} method is
  quite slow, and the results are similar to those of the boxcar
  method.)

  \emph{NB.} A sort of numerical cabeling effect can result from this
procedure, but it can be avoided as follows

\preformatted{
xd <- ctdDecimate(x)
xd[["sigmaTheta"]] <- swSigmaTheta(xd[["salinity"]],xd[["temperature"]],xd[["pressure"]])
} % preformatted
} %details

\value{An object of \code{\link[base]{class}} \code{"ctd"}, with pressures that are as set by the \code{"p"} parameter. In other respects, the return value is identical with the input \code{ctd} object (apart from its having an additional log entry indicating that the data have been altered.)}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(RRprofile)
ctd <- as.ctd(RRprofile$salinity,RRprofile$temperature,RRprofile$depth)
plotProfile(ctd, "T", Tlim=c(2,9))
ctd2 <- ctdDecimate(ctd,p=seq(0,2000,1))
lines(temperature(ctd2), pressure(ctd2), col="blue")
}

\references{R.F. Reiniger and C.K. Ross, 1968.  A method of
interpolation with application to oceanographic data.  \emph{Deep Sea
Research}, \bold{15}, 185-193.}

\author{Dan Kelley}
\keyword{misc}

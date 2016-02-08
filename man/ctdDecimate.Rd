% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{ctdDecimate}

\alias{ctdDecimate}

\title{Decimate a CTD profile}

\description{Interpolate a CTD profile to specified pressure values.}

\usage{ctdDecimate(x, p=1, method="boxcar", e=1.5, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{a \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}

  \item{p}{pressure increment, or vector of pressures.  In the first case,
    pressures from 0dbar to the rounded maximum pressure are used, incrementing
    by \code{p} dbars.  If a vector of pressures is given, interpolation is done
    to these pressures.}
  
  \item{method}{the method to be used for calculating decimated values.  This
    may be a function or a string naming a built-in method.  The built-in
    methods are \code{"boxcar"} (based on a local average), \code{"approx"}
    (based on linear interpolation between neighboring points), \code{"lm"}
    (based on local regression, with \code{e} setting the size of the local
    region), \code{"rr"} (for the Reineger and Ross method, carried out with
    \code{\link{oce.approx}}) and \code{"unesco"} (for the UNESCO method,
    carried out with.  \code{\link{oce.approx}}.  If \code{method} is a
    function, then it must take three arguments, the first being pressure, the
    second being an arbitrary variable in another column of the data, and the
    third being a vector of target pressures at which the calculation is carried
    out, and the return value must be a vector.  See \dQuote{Examples}.}

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

\details{The \code{"approx"} method is best for bottle data, in which the usual
  task is to interpolate from a coarse sampling grid to a finer one. For CTD
  data, the \code{"boxcar"} method is the more common choice, because the task
  is normally to sub-sample, and some degree of smoothing is usually desired.
  (The \code{"lm"} method is quite slow, and the results are similar to those of
  the boxcar method.)

  \emph{NB.} A sort of numerical cabeling effect can result from this
  procedure, but it can be avoided as follows

\preformatted{
xd <- ctdDecimate(x)
xd[["sigmaTheta"]] <- swSigmaTheta(xd[["salinity"]],xd[["temperature"]],xd[["pressure"]])
} % preformatted
} %details

\value{An object of \code{\link[base]{class}} \code{"ctd"}, with pressures that
  are as set by the \code{"p"} parameter and all other properties modified
  appropriately.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure of
  CTD objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(ctd)
plotProfile(ctd, "salinity", ylim=c(10, 0))
p <- seq(0, 45, 1)
ctd2 <- ctdDecimate(ctd, p=p)
lines(ctd2[["salinity"]], ctd2[["pressure"]], col="blue")
p <- seq(0, 45, 1)
ctd3 <- ctdDecimate(ctd, p=p, method=function(x,y,xout)
                    predict(smooth.spline(x, y, df=30), p)$y)
lines(ctd3[["salinity"]], ctd3[["pressure"]], col="red")
}

\references{R.F. Reiniger and C.K. Ross, 1968.  A method of interpolation with
  application to oceanographic data.  \emph{Deep Sea Research}, \bold{15},
  185-193.}

\author{Dan Kelley}

\keyword{misc}

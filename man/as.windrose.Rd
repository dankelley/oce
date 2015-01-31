\name{as.windrose}
\alias{as.windrose}
\title{Create a windrose object}
\description{Create a wind-rose object, typically for plotting with plot.windrose().}

\usage{as.windrose(x, y, dtheta = 15, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{the x component of wind speed (or stress) \emph{or} an object of
      class \code{met} (see \code{\link{met-class}}), in which case the \code{u} and
      \code{v} components of that object are used for the components of wind
      speed, and \code{y} here is ignored.}
  \item{y}{the y component of wind speed (or stress).}
  \item{dtheta}{the angle increment (in degrees) within which to classify the data}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
}
\details{This is analagous to a histogram, but with breaks being angles.}

\value{An object of \code{\link[base]{class}} \code{"windrose"},
    which contains the standard \code{oce} slots named \code{data}, \code{metadata} and 
    \code{proxessingLog}.  The \code{data} slot contains
    \describe{
        \item{\code{n}}{the number of x values}
        \item{\code{x.mean}}{the mean of the x values}
        \item{\code{y.mean}}{the mean of the y values}
        \item{\code{theta}}{the central angle (in degrees) for the class}
        \item{\code{count}}{the number of observations in this class}
        \item{\code{mean}}{the mean of the observations in this class}
        \item{\code{fivenum}}{the \code{\link{fivenum}} vector for
            observations in this class (the min, the lower hinge, the
            median, the upper hinge, and the max)}
    }
}

\seealso{Use \code{\link{plot.windrose}} to produce a summary plot, and
  \code{\link{summary.windrose}} to produce a numerical summary.}

\examples{
library(oce)
xcomp <- rnorm(360) + 1
ycomp <- rnorm(360)
wr <- as.windrose(xcomp, ycomp)
summary(wr)
plot(wr)
}

\author{Dan Kelley, with considerable help from Alex Deckmyn.}

\keyword{misc}

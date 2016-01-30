\name{velocityStatistics}
\alias{velocityStatistics}

\title{Report Statistics of adp or adv Velocities}

\description{Report statistics of ADP or ADV velocities, such as means and
    variance ellipses.}

\usage{velocityStatistics(x, control, \dots)}

\arguments{
  \item{x}{an ADV or ADP object.}

  \item{control}{an optional list used to specify more information.  In this
      version, this is ignored for ADV objects.  For ADP objects, if
      \code{control$bin} is an integer, it is taken as the bin to be selected
      (otherwise, an average across bins is used).}
  \item{\dots}{additional arguments that are used in the call to
      \code{\link{mean}}.} }
  
\details{This function has been tested on only a single file, and the
    data-scanning algorithm was based on visual inspection of that file.
    Whether it will work generally is an open question.}

\value{A list containing items the major and minor axes of the covariance ellipse
    (\code{ellipseMajor} and \code{ellipseMinor}), the angle of the major axis
    anticlockwise of the horizontal axis (\code{ellipseAngle}), and the x and y
    components of the mean velocity (\code{uMean} and \code{vMean}).}

\examples{
library(oce)
data(adp)
a <- velocityStatistics(adp)
print(a)
t <- seq(0, 2*pi, length.out=100)
theta <- a$ellipseAngle * pi / 180
y <- a$ellipseMajor * cos(t) * sin(theta) + a$ellipseMinor * sin(t) * cos(theta)
x <- a$ellipseMajor * cos(t) * cos(theta) - a$ellipseMinor * sin(t) * sin(theta)
plot(adp, which="uv+ellipse+arrow")
lines(x, y, col='blue', lty="dashed", lwd=5)
arrows(0, 0, a$uMean, a$vMean, lwd=5, length=1/10, col='blue', lty="dashed")
}

\author{Dan Kelley}

\keyword{misc}

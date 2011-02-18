\name{angle.remap}

\alias{angle.remap}

\title{Convert an angle from the 0:360 range to -180:180}

\description{Convert an angle from the 0:360 range to -180:180}

\usage{angle.remap(theta)}

\arguments{
    \item{theta}{an angle (in degrees) that is in the range from 0 to 360
        degrees}
}

\details{This is mostly used for instrument heading angles, in cases
    where the instrument is aligned nearly northward, so that small
    variations in heading (e.g. due to mooring motion) can yield values
    that swing from small angles to large angles, because of the
    modulo-360 cut point.

    The method is to use the cosine and sine of the angle, to construct
    "x" and "y" values on a unit circle,  then to find means and
    medians of x and y respectively, and finally to use
    \code{\link{atan2}} to infer the angles.
}

\value{A vector of angles, in the range -180 to 180.}

\examples{
library(oce)
## fake some heading data that lie near due-north (0 degrees)
n <- 20
heading <- 360 + rnorm(n, sd=10)
heading <- ifelse(heading > 360, heading - 360, heading)
x <- 1:n
plot(x, heading, ylim=c(-10, 360), type='l', col='lightgray', lwd=10)
lines(x, angle.remap(heading))
}

\author{Dan Kelley}

\keyword{misc}

\name{beamAttenuateAdp}

\alias{beamAttenuateAdp}

\title{Attenuate ADP beam signal strength}

\description{Attenuate ADP beam signal strength}

\usage{beamAttenuateAdp(x, count2db=c(0.45, 0.45, 0.45, 0.45), debug=getOption("oceDebug"))}

\arguments{
    \item{x}{an object of class \code{"adp"}}
    \item{count2db}{a set of coefficients, one per beam, to convert from
        beam echo intensity to decibels.} 
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.}
}

\details{Beam echo intensity is first converted from counts to decibels,
    by multiplying by \code{count2db}.  Then the signals are range
    corrected by adding the term \eqn{20\log10(r)}{20*log10(r)}, where
    \eqn{r}{r} is the distance from the sensor head to the water from
    which scattering is occuring.  \eqn{r}{r} is given by
    \code{x$data$ss$distance}.
}

\value{An object of \code{\link[base]{class}} \code{"adp"}.}

\examples{
library(oce)
data(adp)
plot(adp, which=5) # beam 1 echo intensity
adp.att <- beamAttenuateAdp(adp)
plot(adp.att, which=5) # beam 1 echo intensity
## Profiles
par(mar=c(4, 4, 1, 1))
plot(apply(adp$data$ma$a,2,mean),adp$data$ss$distance,type='l',xlim=c(0,256))
lines(apply(adp.att$data$ma$a,2,mean),adp$data$ss$distance,type='l',col='red')
legend("topright",lwd=1,col=c("black","red"),legend=c("original","attenuated"))
## Image
plot(adp.att, which="amplitude",col=oceColorsJet(100))
}

\references{The coefficient to convert to decibels is a personal
    communication.  The logarithmic term is explained in textbooks on
    acoustics, optics, etc.}

\author{Dan Kelley}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
    of class \code{"adp"}.}

\keyword{misc}

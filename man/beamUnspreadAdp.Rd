\name{beamUnspreadAdp}

\alias{beamUnspreadAdp}

\title{Adjust ADP signal for spherical spreading}

\description{Compensate ADP signal strength for spherical spreading}

\usage{beamUnspreadAdp(x, count2db=c(0.45, 0.45, 0.45, 0.45),
asMatrix=FALSE, debug=getOption("oceDebug"))}

\arguments{
    \item{x}{an object of class \code{"adp"}}
    \item{count2db}{a set of coefficients, one per beam, to convert from
        beam echo intensity to decibels.} 
    \item{asMatrix}{a boolean that indicates whether to return a numeric matrix, as 
        opposed to returning an updated object (in which the matrix is cast to
        a raw value).}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.}
}

\details{First, beam echo intensity is converted from counts to decibels, by
    multiplying by \code{count2db}.  Then, the signal decrease owing to
    spherical spreading is compensated for by adding the term
    \eqn{20\log10(r)}{20*log10(r)}, where \eqn{r}{r} is the distance from the
    sensor head to the water from which scattering is occuring.  \eqn{r}{r} is
    given by \code{x[["distance"]]}.}

\value{An object of \code{\link[base]{class}} \code{"adp"}.}

\examples{
library(oce)
data(adp)
plot(adp, which=5) # beam 1 echo intensity
adp.att <- beamUnspreadAdp(adp)
plot(adp.att, which=5) # beam 1 echo intensity
## Profiles
par(mar=c(4, 4, 1, 1))
a <- adp[["a", "numeric"]]             # second arg yields matrix return value
distance <- adp[["distance"]]
plot(apply(a,2,mean), distance, type='l', xlim=c(0,256))
lines(apply(a,2,median), distance, type='l',col='red')
legend("topright",lwd=1,col=c("black","red"),legend=c("original","attenuated"))
## Image
plot(adp.att, which="amplitude",col=oce.colorsJet(100))
}

\references{The coefficient to convert to decibels is a personal communication.
    The logarithmic term is explained in textbooks on acoustics, optics, etc.}

\author{Dan Kelley}

\seealso{See \code{\link{read.adp}} for other functions that relate to objects
    of class \code{"adp"}.}

\keyword{misc}


\name{binmapAdp}

\alias{binmapAdp}

\title{Bin-map an ADP object}

\description{Bin-map an ADP object, by interpolating velocities, backscatter
    amplitudes, etc., to uniform depth bins, thus compensating for the pitch
    and roll of the instrument.  This only makes sense for ADP objects that are
    in beam coordinates.}

\usage{binmapAdp(x, debug=getOption("oceDebug"))}

\arguments{
    \item{x}{an object of class \code{"adp"}}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.}
}

\details{This is a preliminary function that is still undergoing testing.  Once
    the methods have been tested more, efforts may be made to speed up the
    processing, either by vectorizing in R or by doing some of the calculation
    in C.}

\section{Bugs}{This only works for 4-beam RDI ADP objects.}

\value{An object of \code{\link[base]{class}} \code{"adp"}.}

\examples{
\dontrun{
library(oce)    
beam <- read.oce("adp_rdi_2615.000",
                 from=as.POSIXct("2008-06-26", tz="UTC"),
                 to=as.POSIXct("2008-06-26 00:10:00", tz="UTC"),
                 longitude=-69.73433, latitude=47.88126)
beam2 <- binmapAdp(beam)
plot(enuToOther(toEnu(beam), heading=-31.5))
plot(enuToOther(toEnu(beam2), heading=-31.5))
plot(beam, which=5:8) # backscatter amplitude
plot(beam2, which=5:8)
}
}

\references{The method was devised by Clark Richards for use in his PhD work at
    Department of Oceanography at Dalhousie University.}
    
\author{Dan Kelley and Clark Richards}

\seealso{See \code{\link{adp-class}} for a discussion of \code{adp} objects and
    notes on the many functions dealing with them.}

\keyword{misc}

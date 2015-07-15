\name{sectionGrid}

\alias{sectionGrid}

\title{Grid a section}

\description{Grid a section, by interpolating to fixed pressure levels.}

\usage{sectionGrid(section, p, method="approx", debug=getOption("oceDebug"), \dots)}

\arguments{

  \item{section}{a \code{section} object containing the section to be gridded.}

  \item{p}{optional indication of the pressure levels to which interpolation
      should be done.  If this is not supplied, the pressure levels will be
      calculated based on the typical spacing in the ctd profiles stored within
      \code{section}.  If \code{p="levitus"}, then pressures will be set to be
      those of the Levitus atlas, given by \code{\link{standardDepths}},
      trimmed to the maximum pressure in \code{section}.  If \code{p} is a
      single numerical value, it is taken as the number of subdivisions to use
      in a call to \code{\link{seq}} that has range from 0 to the maximum
      pressure in \code{section}.  Finally, if a vector numerical values is
      provided, then it is used as is.}

  \item{method}{the method to use to decimate data within the stations; see
      \code{\link{ctdDecimate}}, which is used for the decimation.}

  \item{debug}{a flag that turns on debugging.  The value indicates the depth
      within the call stack to which debugging applies.  For example,
      \code{read.adv.nortek()} calls \code{read.header.nortek()}, so that
      \code{read.adv.nortek(...,debug=2)} provides information about not just
      the main body of the data file, but also the details of the header.}

  \item{\dots}{optional arguments to be supplied to \code{\link{ctdDecimate}}.}

}

\details{The \code{"approx"}, \code{"boxcar"} and \code{"lm"} methods are
    described in the documentation for \code{\link{ctdDecimate}}, which is used
    to do this processing.  The default \code{"approx"} method is best for
    bottle data, the \code{"boxcar"} is best for ctd data, and the \code{"lm"}
    method is probably too slow to recommend for exploratory work, in which it
    is common to do trials with a variety of \code{"p"} values.  }

\value{An object of \code{\link[base]{class}} \code{"section"} that contains
    stations whose pressure values match identically.}

\seealso{The documentation for \code{\link{section-class}} explains the
    structure of section objects, and also outlines the other functions dealing
    with them.}

\examples{
# Gulf Stream
library(oce)
data(section)
GS <- subset(section, 109<=stationId&stationId<=129)
GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
plot(GSg, map.xlim=c(-80,-60))
}

\author{Dan Kelley}

\keyword{misc}

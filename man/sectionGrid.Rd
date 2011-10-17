\name{sectionGrid}

\alias{sectionGrid}

\title{Grid a section}

\description{Grid a section, by interpolating to fixed pressure levels.}

\usage{sectionGrid(section, p, method=c("approx","boxcar","lm"), 
             debug=getOption("oceDebug"), \dots)}

\arguments{
  \item{section}{a \code{section} object containing the section to be gridded.}
  \item{p}{optional indication of the pressure levels to which interpolation should be done.  If this is not supplied, the pressure levels will be calculated based on the typical spacing in the ctd profiles stored within \code{section}.  If a single numerical value is provided, it is taken as a step for a seq() running from 0 to the maximum pressure (rounded to that step).  If \code{p="levitus"}, then the levels of the Levitus atlas are used.  If a list of numerical values is provided, then it is used as is.}
  \item{method}{the algorithm to use for interpolation to the pressure levels; see Details below for a sketch, and see \code{\link{ctdDecimate}} for a deeper discussion.}
  \item{debug}{a flag that turns on debugging.  The value indicates the
    depth within the call stack to which debugging applies.  For
    example, \code{read.adv.nortek()} calls \code{read.header.nortek()},
    so that \code{read.adv.nortek(...,debug=2)} provides information
    about not just the main body of the data file, but also the details
    of the header.}
  \item{\dots}{optional arguments to be supplied to
    \code{\link{ctdDecimate}}.}
}
\details{
The \code{"approx"}, \code{"boxcar"} and \code{"lm"} methods are described in the documentation for \code{\link{ctdDecimate}}, which is used to do this processing.  The default \code{"approx"} method is best for bottle data, the \code{"boxcar"} is best for ctd data, and the \code{"lm"} method is probably too slow to recommend for exploratory work, in which it is common to do trials with a variety of \code{"p"} values.
}

\value{An object of \code{\link[base]{class}} \code{"section"} that
  contains stations whose pressure values match identically.}

\seealso{Sections are normally created with \code{\link{read.section}} or
    \code{\link{makeSection}}, and smoothed with \code{\link{sectionSmooth}}.
    The generics \code{subset}, \code{plot} and \code{summary} methods work,
    also.}

\examples{
# Gulf Stream
library(oce)
# File source -- http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1
# a03 <- read.section("a03_hy1.csv")
data(a03)
GS <- subset.oce(a03, indices=124:102)
GSg <- sectionGrid(GS, p=seq(0, 5000, 100))
data(coastlineWorld)
plot(GSg, coastline=coastlineWorld, map.xlim=c(-80,-60))
}

\author{Dan Kelley}

\keyword{misc}

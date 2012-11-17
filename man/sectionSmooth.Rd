\name{sectionSmooth}

\alias{sectionSmooth}

\title{Smooth a section (preliminary version that may change)}

\description{Smooth a section in the lateral (alpha version that may change)}

\usage{sectionSmooth(section, method=c("spline", "barnes"), debug=getOption("oceDebug"), ...)}

\arguments{
    \item{section}{a \code{section} object containing the section to be smoothed.
        For \code{method="spline"}, the pressure levels must match for each
        station in the section.}
    \item{method}{specifies the method to use; see \sQuote{Details}.}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.}
    \item{\dots}{optional extra arguments, passed to either
        \code{\link{smooth.spline}} or \code{\link{interpBarnes}}.}
}

\details{For \code{method="spline"}, the section is smoothed using
    \code{\link{smooth.spline}} on individual pressure levels, with any
    parameters listed in \code{parameters} being passed to that function.  If
    \code{df} is not present in \code{parameters}, then this function sets it
    to the number of stations divided by 5.  Smoothing is done separately for
    temperature, salinity, and sigma-theta.

    For \code{method="barnes"}, \code{\link{interpBarnes}} is used to construct
    a grid, with any arguments in \code{\dots} being passed to that function;
    see \sQuote{Examples}.

    The \code{barnes} method is generally an order of magnitude slower than the
    \code{spline} method, but it has the advantage of being closer to a standard.

}

\value{An object of \code{\link[base]{class}} \code{"section"} that
has less lateral variation than the input section.}

\seealso{The documentation for \code{\link{section-class}} explains the
    structure of section objects, and also outlines the other functions dealing
    with them.}

\examples{
library(oce)
data(coastlineWorld)
data(a03)
gs <- sectionGrid(subset.oce(a03, indices=124:102), p=seq(0, 5000, 150))
gss1 <- sectionSmooth(gs, "spline", df=16)
plot(gss1, coastline=coastlineWorld, map.xlim=c(-80,-55))
gss2 <- sectionSmooth(gs, "barnes", xr=24, yr=100)
plot(gss2, coastline=coastlineWorld, map.xlim=c(-80,-55))
}

\author{Dan Kelley}

\keyword{misc}

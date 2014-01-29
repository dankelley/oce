\name{plot.lisst}

\alias{plot.lisst}
\alias{plot,lisst,missing-method}
\alias{plot,lisst-method}

\title{Plot LISST data}

\description{Plot \code{LISST} data}

\usage{\S4method{plot}{lisst}(x, which=c(16, 37, 38), tformat, debug=getOption("oceDebug"), \dots)}

\arguments{
    \item{x}{a \code{lisst} object, e.g. as read by \code{\link{read.lisst}}.}
    \item{which}{list of desired plot types.  These are graphed in
        panels running down from the top of the page.  See \dQuote{Details}
        for the meanings of various values of \code{which}.}
    \item{tformat}{optional argument passed to \code{\link{oce.plot.ts}}, for
        plot types that call that function.  (See \code{\link{strptime}} for the
        format used.)}
    \item{debug}{a flag that turns on debugging.  The value indicates the
        depth within the call stack to which debugging applies.}
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates a multi-panel summary plot of data measured by LISST
    instrument.  The panels are controlled by the \code{which} argument, as
    follows.
    \itemize{ 

        \item \code{which=1} to \code{32}, or \code{which="C1"} to \code{"C32"}
        for a time-series graph of the named column (a size class). 

        \item \code{which=33} or \code{which="lts"} for a time-series plot
        of laser transmission sensor.

        \item \code{which=34} or \code{which="voltage"} for a time-series plot
        of instrument voltage.

        \item \code{which=35} or \code{which="aux"} for a time-series plot
        of the external auxiliary input.

        \item \code{which=36} or \code{which="lrs"} for a time-series plot
        of the laser reference sensor.

        \item \code{which=37} or \code{which="pressure"} for a time-series plot
        of pressure.

        \item \code{which=38} or \code{which="temperature"} for a time-series plot
        of temperature.

        \item \code{which=41} or \code{which="transmission"} for a time-series plot
        of transmission, in percent.

        \item \code{which=42} or \code{which="beam"} for a time-series plot
        of beam-C, in 1/metre.
    }
}

\examples{
library(oce)
if (require(ocedata)) {
    data(lisst)
    plot(lisst)
}
}

\seealso{The documentation for \code{\link{lisst-class}} explains the structure
    of lisst objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}
\keyword{hplot}

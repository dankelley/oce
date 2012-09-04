% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{riley}

\docType{data}

\alias{riley}

\title{Riley's (1946) observation and theory of phytoplankton concentration}

\description{Riley's (1946) observation and theory of phytoplankton
    concentration.   The list \code{riley} contains three entries.

    \itemize{

        \item \code{riley$fig21points} is a data frame representing the data shown in Riley's Figure
        21.  Resulting from digitization of his graph, this is a list containing \code{day} (day in
        year) and \code{P} (phytoplankton concentraiton, in grams of Carbon per square meter.

        \item \code{riley$fig21curve} is a data frame with columns named \code{day} and \code{P},
        containing a digitized trace of the curve with which Riley illustrates his numerical
        solution of the differential equations for phytoplankton growth.
        
        \item \code{DEparameters} is a list containing data about twice per month, in elements
        \code{day} for the day of the year, \code{Ph} for phytoplankton growth rate, \code{R} for
        phytoplankton respiration rate, and \code{G} for the rate at which zooplankton graze on
        phytoplankton.  The rates are in inverse days.

    }
    
    This is used in Chapter 4 of Kelley (2012).}

\usage{data(riley)}

\examples{
library(oce)
data(riley)
plot(riley$fig21points$day, riley$fig21points$P, xlab="Day of Year", ylab="Phytoplankton [gC/m^2]")
lines(riley$fig21curve$day, riley$fig21curve$P)
}

\source{\code{riley$fig21points} is based on manual manual digitization of
    Riley's Figure 21.  \code{riley$fig21curve} are based on manual
    digitization of Riley's theoretical curve in the same figure, fitted to a
    spline with \code{\link{splinefun}}, which yields a function that can be
    used to predict at any time.  \code{riley$DEparameters} was transcribed from
    Riley's appendix.}

\references{
    Dan Kelley, in preparation. Oceanographic Analysis with R.  Springer Verlag.

    Gordon A. Riley, 1946.  Factors controlling phytoplankton populations on Georges Bank.
    Journal of Marine Research, 6(1): 54-73.

}

\keyword{datasets}

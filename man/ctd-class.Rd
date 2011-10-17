\name{ctd-class}

\docType{class}

\alias{ctd-class}

\title{Class to store hydrographic data, e.g. from a CTD}

\description{Class to store hydrographic data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\examples{
a <- new('ctd', pressure=c(1.5, 13.7, 25.7, 37.7),
          salinity=c(29.92, 31.27, 31.44, 31.45),
          temperature= c(14.2, 9.0, 7.2, 4.0))
str(a)
}
\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


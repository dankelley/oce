\name{coastlineBest}

\alias{coastlineBest}

\title{Find best coastline file}

\description{Find highest-resolution coastline for given locale}

\usage{coastlineBest(lonRange, latRange, debug=getOption("oceDebug"))}


\arguments{

  \item{lonRange}{range of longitude for locale}
  
  \item{latRange}{range of latitude for locale}
  
  \item{debug}{set to a positive value to get debugging information during processing.}

}
  
\details{Checks \code{coastlineHalifax}, \code{coastlineMaritimes} and
    \code{coastlineWorld}, in that order, to find the first that spans the
    locale.  This yields the highest resolution.}

\value{A coastline}

\author{Dan Kelley}

\keyword{misc}

\name{coastlineBest}

\alias{coastlineBest}

\title{Find best coastline file}

\description{Find highest-resolution coastline for given locale}

\usage{coastlineBest(lonRange, latRange, span,
    debug=getOption("oceDebug"))}


\arguments{

  \item{lonRange}{range of longitude for locale}
  
  \item{latRange}{range of latitude for locale}
  
  \item{span}{span of domain in km (if provided, previous two arguments are
      ignored).}
  
  \item{debug}{set to a positive value to get debugging information during
      processing.}

}
  
\details{Checks \code{coastlineWorld}, \code{coastlineWorldFine} and
    \code{coastlineWorldCoarse}, in that order, to find the one most
    appropriate for the locale.}

\value{A coastline}

\author{Dan Kelley}

\keyword{misc}

\name{coastlineBest}

\alias{coastlineBest}

\title{Find the name of the best coastline file}

\description{Find the name of the most appropriate coastline for a given
    locale}

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

\value{The name of a coastline that can be loaded with \code{data()}.}

\author{Dan Kelley}

\keyword{misc}

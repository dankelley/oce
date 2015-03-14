\name{sealevel-class}

\docType{class}

\alias{sealevel-class}
\alias{[[,sealevel-method}
\alias{[[<-,sealevel-method}
\alias{show,sealevel-method}

\title{Class to store sealevel data}

\description{Class to store sealevel data, e.g. from a tide gauge, with
    standard slots \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{
    Data may be accessed as e.g.  \code{sealevel[["time"]]}, where the string could
    also be e.g.  \code{"elevation"} for the corresponding sea-level elevation,
    or e.g.  \code{"longitude"} or \code{"latitude"} for scalars.  Items in
    \code{metadata} must be specifield by full name, but those in \code{data}
    may be abbreviated, so long as the abbreviation is unique.

    Everything that may be accessed may also be assigned, e.g.
    \code{sealevel[["elevation"]] <- value}.
    
    The \code{show} method displays information about the object, while
    \code{\link{summary.sealevel}} provides a statistical summary.  }

\examples{
library(oce)
data(sealevel)
show(sealevel)
plot(sealevel)
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


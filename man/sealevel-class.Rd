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
    or e.g.  \code{"longitude"} or \code{"latitude"} for scalars.  (The names
    of the columns are displayed with \code{show()}.) The name of the source
    file is found with \code{"filename"}.

    Everything that may be accessed may also be assigned, e.g.
    \code{sealevel[["elevation"]] <- value}.
    
    The \code{show} method displays information about the object, while
    \code{\link{summary.sealevel}} provides a statistical summary.  }

\seealso{The documentation for \code{\link{sealevel-class}} explains the
    structure of sealevel objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
if (require(ocedata)) {
    data(sealevelHalifax)
    show(sealevelHalifax)
    plot(sealevelHalifax)
}
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


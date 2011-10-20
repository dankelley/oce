\name{section-class}

\docType{class}

\alias{section-class}
\alias{[[,section-method}
\alias{[[<-,section-method}
\alias{show,section-method}

\title{Class to store hydrographic section data}

\description{Class to store hydrographic section data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{

    \subsection{Extracting values}{Individual stations may be accessed as e.g.
        \code{section[["station", 1]]} for the first station (which returns a
        \code{\link{ctd-class}} object; see the example).  Aggregated values of
        measured quantities may be accessed as e.g.
        \code{section[["salinity"]]}, which returns a vector containing every
        salinity value in every station.  
        
        FIXME: invent a scheme for getting either station-by-station latitudes,
        or aggregated values that correspond to the salinities; both things are
        useful.  (Possibly have the metadata be called \code{latitudes}, and
        get it that way?)}

    \subsection{Assigning values}{FIXME: fill this in when the interface is
        settled.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(ctd)}) displays information about the object.}
}

\examples{
library(oce)
data(a03)
plot(a03[['station', 1]])
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


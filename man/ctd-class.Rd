\name{ctd-class}

\docType{class}

\alias{ctd-class}
\alias{[[,ctd-method}
\alias{[[<-,ctd-method}

\title{Class to store hydrographic data, e.g. from a CTD}

\description{Class to store hydrographic data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{ctd[["salinity"]]}, where
    the string could also be \code{"temperature"} or \code{"pressure"} for
    column data, or \code{"latitude"} or \code{"longitude"} for scalars.  The
    filename is found with \code{"filename"}.  
    
    Everything that may be accessed may also e assigned, e.g.
    \code{ctd[["salinity"]] <- rep(35,10)}.
    
    All of the names may be abbreviated, so long as they are unique, e.g.
    \code{ctd[["s"]]} provides the salinity values.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


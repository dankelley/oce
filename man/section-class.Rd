\name{section-class}

\docType{class}

\alias{section-class}
\alias{[[,section-method}

\title{Class to store hydrographic section data}

\description{Class to store hydrographic section data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{section[["salinity"]]},
    where the string could also be \code{"temperature"} or \code{"pressure"}
    for a compilation of the data across all of the stations.  The location of
    the stations (just one value per station) is given with strings
    \code{"latitude"} or \code{"longitude"}.  The filename is given by
    \code{"filename"}. (Abbreviations for any of these are permissable, so long
    as they are unique.)}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


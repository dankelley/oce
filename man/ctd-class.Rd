\name{ctd-class}

\docType{class}

\alias{ctd-class}
\alias{[[,ctd-method}

\title{Class to store hydrographic data, e.g. from a CTD}

\description{Class to store hydrographic data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{ctd[["salinity"]]}, where
    the string could also be \code{"temperature"} or \code{"pressure"} for
    column data, or \code{"latitude"} or \code{"longitude"} for scalars.
    The filename is found with \code{"filename"}.
    (Abbreviations for any of these are permissable, so long as they are
    unique.)}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


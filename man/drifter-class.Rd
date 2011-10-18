\name{drifter-class}

\docType{class}

\alias{drifter-class}
\alias{[[,drifter-method}

\title{Class to store drifter data, e.g. from an ARGOS float}

\description{Class to store drifter data, with standard slots \code{metadata},
    \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{drifter[["salinity"]]},
    where the string could also be \code{"temperature"}, \code{"pressure"},
    \code{"latitude"}, \code{"longitude"}, or \code{"time"}, or abbreviations
    that match uniquely to any of these possibilities.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


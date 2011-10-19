\name{adp-class}

\docType{class}

\alias{adp-class}
\alias{[[,adp-method}

\title{Class to store acoustic Doppler profiler data}

\description{Class to store acoustic Doppler proviler data, with standard
    slots \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Velocity data may be accessed as e.g. \code{adp[["u1"]]} for
    the x component if in frame coordinates, or eastward velocity if in ENU
    coordinates), with \code{"u2"} for the second component, etc.  These are
    all 2D matrices for measurements across time and distance.  Time,
    temperature and pressure data are given by \code{adp[["time"]]},
    \code{adp[["temperature"]]} and \code{adp[["pressure"]]}, which produce
    vectors of equal length.  \code{adp[["distance"]]} gives the distances of
    the bins.  Location data may be accessed with \code{adp[["latitude"]]} and
    \code{adp[["longitude"]]}, which return scalars.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


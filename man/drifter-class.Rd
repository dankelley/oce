\name{drifter-class}

\docType{class}

\alias{drifter-class}
\alias{[[,drifter-method}

\title{Class for drifter data}

\description{Class to store drifter data, with standard slots \code{metadata},
    \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{pt[["time"]]}, where the
    string could also be \code{"longitude"} or \code{"latitude"}.  The data
    data are available in \code{"salinity"}, \code{"temperature"} and
    \code{"pressure"}, each of which is a matrix with first index corresponding
    to scan number and the second to profile number.}

\seealso{A \code{drifter} object may be read with \code{\link{read.drifter}} or
    created with \code{\link{as.drifter}}.  Drifter data can be gridded to
    constant pressures with \code{\link{drifterGrid}}.  Plots can be made with
    \code{\link{plot.drifter}}, while \code{\link{summary.drifter}} produces
    statistical summaries and \code{show} produces overviews.  See
    \url{http://www.argo.ucsd.edu/Gridded_fields.html} for some drifter-related
    datasets that may be useful in a wider context.}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


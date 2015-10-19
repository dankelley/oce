\name{argo-class}

\docType{class}

\alias{argo-class}
\alias{[[,argo-method}

\title{Class for argo data}

\description{Class to store data from Argo drifers [1], with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{pt[["time"]]}, where the
    string could also be \code{"longitude"} or \code{"latitude"}.  The data
    data are available in \code{"salinity"}, \code{"temperature"} and
    \code{"pressure"}, each of which is a matrix with first index corresponding
    to scan number and the second to profile number.  Items in \code{metadata}
    must be specifield by full name, but those in \code{data} may be
    abbreviated, so long as the abbreviation is unique.  }

\seealso{A \code{argo} object may be read with \code{\link{read.argo}} or
    created with \code{\link{as.argo}}.  Drifter data can be gridded to
    constant pressures with \code{\link{argoGrid}}.  Plots can be made with
    \code{\link{plot.argo}}, while \code{\link{summary.argo}} produces
    statistical summaries and \code{show} produces overviews.  See
    \url{http://www.argo.ucsd.edu/Gridded_fields.html} for some argo-related
    datasets that may be useful in a wider context.}

\section{Historical note}{This class was named \code{"drifter"} until September
    24, 2015, but the name was changed to reflect increasing specialization for
    the particular type of drifter used in the Argo program.}

\author{Dan Kelley}

\keyword{classes}

\references{
    1. Argo program: \url{http://www.argo.ucsd.edu}
}

\keyword{oce}


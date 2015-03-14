\name{lobo-class}

\docType{class}

\alias{lobo-class}
\alias{[[,lobo-method}
\alias{[[<-,lobo-method}
\alias{show,lobo-method}

\title{Class to store LOBO data}

\description{Class to store LOBO data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{pt[["time"]]}, where the
    string could also be \code{"pressure"} or \code{"temperature"}.  Items in
    \code{metadata} must be specifield by full name, but those in \code{data}
    may be abbreviated, so long as the abbreviation is unique.  Assignment to
    these can be made with e.g. \code{pt[["pressure"]] <- value}, etc.  Indeed,
    any quantity in the \code{metadata} slot or the \code{data} slot can be
    retrieved or updated in this way.}

\seealso{A \code{lobo} object may be read with \code{\link{read.lobo}} or
    created with \code{\link{as.lobo}}.  Plots can be made with
    \code{\link{plot.lobo}}, while \code{\link{summary.lobo}} produces
    statistical summaries and \code{show} produces overviews.}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


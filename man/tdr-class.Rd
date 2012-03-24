\name{tdr-class}

\docType{class}

\alias{tdr-class}
\alias{[[,tdr-method}
\alias{[[<-,tdr-method}
\alias{show,tdr-method}

\title{Class to store temperature-depth recorder data}

\description{Class to store temperature-depth recorder data, with standard
    slots \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{tdr[["time"]]}, where the
    string could also be \code{"pressure"} or \code{"temperature"}.  Assignment
    to these can be made with e.g. \code{tdr[["pressure"]] <- value}, etc.
    Indeed, any quantity in the \code{metadata} slot or the \code{data} slot
    can be retrieved or updated in this way.}

\seealso{A \code{tdr} object may be read with \code{\link{read.tdr}} or created
    with \code{\link{as.tdr}}.  Plots can be made with \code{\link{plot.tdr}},
    while \code{\link{summary.tdr}} produces statistical summaries and
    \code{show} produces overviews.   If atmospheric pressure has not been
    removed from the data, the functions \code{\link{tdrPatm}} may provide
    guidance as to its value.  Similarly, if the record contains periods when
    the instrument was in the air, \code{\link{tdrTrim}} may prove useful in
    isolating the times when it was in the water.  However, these last two
    functions are no equal to decent record-keeping at sea.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


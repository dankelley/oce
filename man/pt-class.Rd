\name{pt-class}

\docType{class}

\alias{pt-class}
\alias{[[,pt-method}
\alias{[[<-,pt-method}
\alias{show,pt-method}

\title{Class to store pressure-temperature recorder data}

\description{Class to store pressure-temperature recorder data, with standard
    slots \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{Data may be accessed as e.g. \code{pt[["time"]]}, where the
    string could also be \code{"pressure"} or \code{"temperature"}.
    
    Assignment to these can be made with e.g. \code{pt[["pressure"]] <- value},
    etc.

    Indeed, any quantity in the \code{metadata} slot or the \code{data} slot
    can be retrieved or updated in this way.
}

\seealso{A \code{pt} object may be read with \code{\link{read.pt}} or created
    with \code{\link{as.ctd}}.  Plots can be made with \code{\link{plot.pt}},
    while \code{\link{summary.ctd}} produces statistical summaries and
    \code{show} produces overviews.   If atmospheric pressure has not been
    removed from the data, the functions \code{\link{ptPatm}} may provide
    guidance as to its value.  Similarly, if the record contains periods when
    the instrument was in the air, \code{\link{ptTrim}} may prove useful in
    isolating the times when it was in the water.  However, these last two
    functions are no equal to decent record-keeping at sea.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


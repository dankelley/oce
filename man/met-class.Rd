\name{met-class}

\docType{class}

\alias{met-class}
\alias{[[,met-method}
\alias{[[<-,met-method}
\alias{show,met-method}

\title{Class to store meteorological data}

\description{Class to store meteorological data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{

    \emph{Accessing values.} For an object named \code{m}, temperature may be
    accessed as \code{m[["time"]]}, which yields a vector.  Similarly pressure
    is \code{m[["pressure"]]} in kPa.  The components of wind are stored in
    \emph{oceanographic} convention, i.e. the eastward component is available
    as \code{m[["u"]]} and the northward component is \code{m[["v"]]}.
    \code{m[["u"]]}, and wind direction is \code{m[["direction"]]}.  The
    filename from which the data came (if any) may be found with
    \code{m[["filename"]]}.  Items in \code{metadata} must be specifield by
    full name, but those in \code{data} may be abbreviated, so long as the
    abbreviation is unique.
    
    \emph{Assigning values.} Everything that may be accessed may also be
    assigned, e.g.  \code{m[["temperature"]] <- 1 + m[["temperature"]]}
    increases temperature by 1C.

    \emph{Overview of contents.} The \code{show} method (e.g.
    \code{show(met)}) displays information about the object.
}

\seealso{A file containing meteorological data may be read with
    \code{\link{read.met}}, and a meteorological object can also be created
    with \code{\link{as.met}}.  A sample object is available with
    \code{data(\link{met})}.  Statistical summaries are provided by
    \code{\link{summary.met}}, while \code{\link{show}} displays an overview.
    Plotting may be handled with \code{\link{plot.met}}.  }

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


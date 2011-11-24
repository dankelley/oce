\name{echosounder-class}

\docType{class}

\alias{echosounder-class}
\alias{[[,echosounder-method}
\alias{[[<-,echosounder-method}
\alias{show,echosounder-method}

\title{Class to store echosounder data}

\description{Class to store echosounder data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{

    \emph{Accessing values.} Data may be accessed as e.g.
    \code{echosounder[["time"]]}, where the string could also be e.g.  FIXME
    (add more).  The names of the columns are displayed with \code{show().} The
    name of the source file is found with \code{"filename"}.

    Derived data are also available: \code{"distance"} calls
    \code{\link{geodDist}} to compute locations, and calculates distance along
    the ship track.

    \emph{Assigning values.} Everything that may be accessed may also be
    assigned, e.g.  \code{echosounder[["time"]] <- 3600 +
        echosounder[["time"]]} adds an hour to time.

    \emph{Overview of contents.} The \code{show} method (e.g.
    \code{show(echosounder)}) displays information about the object.
}

\seealso{A file containing echosounder data may be read with
    \code{\link{read.echosounder}}, and a echosounder object can also be
    created with \code{\link{as.echosounder}}.
  
  Statistical summaries are provided by \code{\link{summary.echosounder}},
  while \code{\link{show}} displays an overview.
  
  Echosounder objects may be plotted with \code{\link{plot.echosounder}}.

  The contents of \code{echosounder} objects may be altered with the
  \code{[[]]} scheme discussed in the previous section, and skilled uses may
  also manipulate the contents directly.

}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


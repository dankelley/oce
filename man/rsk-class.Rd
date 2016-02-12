% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2

\name{rsk-class}

\docType{class}

\alias{rsk-class}
\alias{[[,rsk-method}
\alias{[[<-,rsk-method}
\alias{show,rsk-method}

\title{Class to store RBR rsk data}

\description{Class for data stored in the ``Ruskin'' format used by RBR
  [1] (including both \code{rsk} SQLite files and the ASCII \code{txt}
  exported files),
  with the standard \code{oce} slots, as follows.\itemize{
    \item{\code{data}}{: a list containing the \code{time}, plus other columns
      for data recorded by the device.}
    \item{\code{metadata}}{: a list containing the information about the data.}
    \item{\code{processingLog}}{: a log of processing steps, in the standard
      \code{oce} format.}
  }
}

\section{Methods}{Data may be accessed as e.g. \code{rsk[["time"]]}, where the
  string could also be \code{"pressure"} or \code{"temperature"}.  Items in
  \code{metadata} must be specifield by full name, but those in \code{data} may
  be abbreviated, so long as the abbreviation is unique.  Assignment can be made
  with e.g. \code{rsk[["pressure"]] <- value}, etc.}

\seealso{A \code{rsk} object may be read with \code{\link{read.rsk}} or created
  with \code{\link{as.rsk}}.  Plots can be made with \code{\link{plot.rsk}},
  while \code{\link{summary.rsk}} produces statistical summaries and \code{show}
  produces overviews.   If atmospheric pressure has not been removed from the
  data, the functions \code{\link{rskPatm}} may provide guidance as to its
  value; however, this last function is no equal to decent record-keeping at
  sea.}

\author{Dan Kelley and Clark Richards}

\references{1. \href{http://www.rbr-global.com/products}{RBR website: www.rbr-global.com/products}}

\keyword{classes}

\keyword{oce}


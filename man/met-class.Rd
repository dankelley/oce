% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{met-class}

\docType{class}

\alias{met-class}
\alias{[[,met-method}
\alias{[[<-,met-method}
\alias{show,met-method}

\title{Class to store meteorological data}

\description{Class to store meteorological data, with standard slots
  \code{metadata}, \code{data} and \code{processingLog}.  For objects created
  with \code{\link{read.met}}, the \code{data} slot will contain all the columns
  within the original file (with some guesses as to units) in addition to
  several calculated quantities such as \code{u} and \code{v}, which are
  velocities in m/s (not the km/h stored in typical data files), and which obey
  the oceanographic convention that \code{u>0} is a wind towards the east.}

\section{Methods}{

  \emph{Accessing values.} For an object named \code{m}, temperature (in degC)
  may be accessed as \code{m[["temperature"]]}, dew point (in degC) as
  \code{m[["dewPoint"]]}, pressure (in kPa) as \code{m[["pressure"]]}, eastward
  wind component (in m/s) as \code{m[["u"]]}, northward wind component (in m/s)
  as \code{m[["v"]]}.  \strong{Caution:} the other elements stored in the
  dataset are mainly in the format of the source file, and thus their use
  requires some extra knowledge; for example, \code{m[["direction"]]} yields the
  wind direction, measured in 10-degree units positive clockwise from North.
  The filename from which the data came (if any) may be found with
  \code{m[["filename"]]}.  Items in \code{metadata} must be specifield by full
  name, but those in \code{data} may be abbreviated, so long as the abbreviation
  is unique.

  \emph{Assigning values.} Everything that may be accessed may also be assigned,
  e.g.  \code{m[["temperature"]] <- 1 + m[["temperature"]]} increases
  temperature by 1C.

  \emph{Overview of contents.} The \code{show} method (e.g.  \code{show(met)})
  displays information about the object.}

\seealso{A file containing meteorological data may be read with
  \code{\link{read.met}}, and a meteorological object can also be created
  with \code{\link{as.met}}.  A sample object is available with
  \code{data(\link{met})}.  Statistical summaries are provided by
  \code{\link{summary.met}}, while \code{\link{show}} displays an overview.
  Plotting may be handled with \code{\link{plot.met}}.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


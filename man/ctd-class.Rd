\name{ctd-class}

\docType{class}

\alias{ctd-class}
\alias{[[,ctd-method}
\alias{[[<-,ctd-method}
\alias{show,ctd-method}

\title{Class to store hydrographic data}

\description{Class to store hydrographic data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{
    \subsection{Extracting values}{Data may be accessed as e.g.
        \code{ctd[["salinity"]]}, where the string could also be e.g.
        \code{"temperature"} or \code{"pressure"} for column data, or
        \code{"latitude"} or \code{"longitude"} for scalars.  (The names of the
        columns are displayed with \code{show().} The name of the source file
        is found with \code{"filename"}.}
        %The names of the entities may be abbreviated, so long as the
        %abbreviations uniquely identify the entity.

    \subsection{Assigning values}{Everything that may be accessed may also be
        assigned, e.g.  \code{ctd[["salinity"]] <- rep(35,10)}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(ctd)}) displays information about the object.}
}

\seealso{A file containing CTD profile data may be read with
  \code{\link{read.ctd}}, and a CTD object can also be created with
  \code{\link{as.ctd}}.
  
  Statistical summaries are provided by \code{\link{summary.ctd}},
  while \code{\link{show}} displays an overview.
  
  CTD objects may be plotted with \code{\link{plot.ctd}}, which does much of
  its work by calling \code{\link{plotProfile}} or \code{\link{plotTS}}, both
  of which can also be called by the user, to get fine control over the plots.

  The editing of CTD objects (e.g. to isolate downcasts) can be done with
  \code{\link{ctdTrim}}, a task made easier when \code{\link{plotScan}} is
  used to examine the results.  CTD data may be smoothed and/or cast onto
  specified pressure levels with \code{\link{ctdDecimate}}.  
  
  In addition to the direct accessors discussed above, low-level manipulation
  may be done with \code{\link{ctdAddColumn}} and
  \code{\link{ctdUpdateHeader}}.  The contents of CTD objects may be altered
  with the \code{[[]]} scheme discussed in the previous section, and skilled
  uses may also manipulate the contents directly.
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


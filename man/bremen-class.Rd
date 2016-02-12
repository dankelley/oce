\name{bremen-class}

\docType{class}

\alias{bremen-class}
\alias{[[,bremen-method}
\alias{[[<-,bremen-method}
\alias{show,bremen-method}

\title{Class for data stored in a format used at Bremen}

\description{Class for data stored in a format used at Bremen. This is somewhat
    unusual amongst \code{oce} classes, in that it does not map to a particular
    instrument, but rather to a storage type; in that sense, it is similar to
    the \code{odf-class}.}

\section{Methods}{

    Consider an object named \code{bremen}.

    \emph{Accessing metadata.}
  
    Metadata (contained in the S4 slot named \code{metadata}) may be retrieved
    or set by name, \code{bremen[["longitude"]] <- bremen[["longitude"]] + 1}
    corrects a one-degree error.  Use \code{names(bremen@metadata)} to find the
    names of the metadata.

    \emph{Accessing measured data.}

    Column data may be accessed by name, e.g. \code{bremen[["u"]]},
    \code{bremen[["v"]]}, \code{bremen[["pressure"]]}, etc.  There may also be
    columns for \code{"temperature"} and \code{"salinity"}, and possibly other
    things. Use \code{names(bremen@data)} to find the names of the data.
    
    \emph{Overview of contents.}
   
    The \code{show} method (e.g.  \code{show(bremen)}) displays information about
    the object. The \code{summary} method shows some statistical information.

}

\seealso{A file containing Bremen profile data may be read with
    \code{\link{read.bremen}}.
  
    Statistical summaries are provided by \code{\link{summary.bremen}}, while
    \code{\link{show}} displays an overview.
  
    Bremen objects may be plotted with \code{\link{plot.bremen}}, although it is more
    common to coerce to another class and plot that.
}

  
\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


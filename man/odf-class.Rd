\name{odf-class}

\docType{class}

\alias{odf-class}
\alias{[[,odf-method}
\alias{[[<-,odf-method}
\alias{show,odf-method}

\title{Class to store ODF data}

\description{Class for data stored in a format used at Canadian Department of
    Fisheries and Oceans laboratories. This is somewhat unusual amongst
    \code{oce} classes, in that it does not map to a particular instrument, but
    rather to a storage type; in that sense, it is similar to the
    \code{bremen-class}.}


\section{Methods}{

    Consider an ODF object named \code{odf}.

    \emph{Accessing metadata.}
  
    Metadata (contained in the S4 slot named \code{metadata}) may be retrieved
    or set by name, \code{odf[["longitude"]] <- odf[["longitude"]] + 1}
    corrects a one-degree error.

    \emph{Accessing measured data.}

    Column data may be accessed by name, e.g. \code{odf[["salinity"]]},
    \code{odf[["temperature"]]}, \code{odf[["pressure"]]}, etc.  It is up to
    the user to realize what is in the object.

    \emph{Assigning values.}
   
    Items stored in the object may be altered with e.g.
    \code{odf[["salinity"]] <- rep(35,10)}.

    \emph{Overview of contents.}
   
    The \code{show} method (e.g.  \code{show(odf)}) displays information about
    the object.

}

\seealso{A file containing ODF profile data may be read with
    \code{\link{read.odf}}.
  
    Statistical summaries are provided by \code{\link{summary.odf}}, while
    \code{\link{show}} displays an overview.
  
    ODF objects may be plotted with \code{\link{plot.odf}}, although it is more
    common to coerce to another class and plot that.
}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


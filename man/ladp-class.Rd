\name{ladp-class}

\docType{class}

\alias{ladp-class}
\alias{[[,ladp-method}
\alias{[[<-,ladp-method}
\alias{show,ladp-method}

\title{Class to store hydrographic data}

\description{Class to store hydrographic data such as measured with a lowered
    ADP (also known as ADCP) device.}

\section{Methods}{

    Consider an object named \code{ladp}.

    \emph{Accessing metadata.}
  
    Metadata (contained in the S4 slot named \code{metadata}) may be retrieved
    or set by name, \code{ladp[["longitude"]] <- ladp[["longitude"]] + 1}
    corrects a one-degree error.  Use \code{names(ladp@metadata)} to find the
    names of the metadata.

    \emph{Accessing measured data.}

    Column data may be accessed by name, e.g. \code{ladp[["u"]]},
    \code{ladp[["v"]]}, \code{ladp[["pressure"]]}, etc.  There may also be
    columns for \code{"temperature"} and \code{"salinity"}, and possibly other
    things. Use \code{names(ladp@data)} to find the names of the data.
    
    \emph{Overview of contents.}
   
    The \code{show} method (e.g.  \code{show(ladp)}) displays information about
    the object. The \code{summary} method shows some statistical information.

}

\seealso{Data may be coerced into an \code{ladp} object with
    \code{\link{as.ladp}}.}
  
\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


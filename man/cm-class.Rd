% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{cm-class}

\docType{class}

\alias{cm-class}
\alias{[[,cm-method}
\alias{[[<-,cm-method}
\alias{show,cm-method}

\title{Class to store current meter data}

\description{Class to store current meter data, with standard slots
  \code{metadata}, \code{data} and \code{processingLog}. See
  \code{\link{read.cm}} for some cautionary notes on this data type.}
 
\section{Methods}{

  \subsection{Extracting values}{Data may be accessed as e.g.
    \code{cm[["time"]]}, \code{"u"}, \code{"v"}, etc.  Items in \code{metadata}
    must be specifield by full name, but those in \code{data} may be
    abbreviated, so long as the abbreviation is unique.}

  \subsection{Assigning values}{Everything that may be accessed may also be
    assigned, e.g.  \code{cm[["u"]] <- rep(35,100)}.}

  \subsection{Overview of contents}{The \code{show} method (e.g.
    \code{show(cm)}) displays information about the object.}

}

\seealso{A file containing CM profile data may be read with
  \code{\link{read.cm}}.  Statistical summaries are provided with
  \code{\link{summary.cm}}, while overviews are provided by \code{show}.  Plots
  are provided with \code{\link{plot.cm}}.}
 
\author{Dan Kelley} \keyword{classes} \keyword{oce}


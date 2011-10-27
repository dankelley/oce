\name{cm-class}

\docType{class}

\alias{cm-class}
\alias{[[,cm-method}
\alias{[[<-,cm-method}
\alias{show,cm-method}

\title{Class to store current meter data}

\description{Class to store current meter data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}
 
\section{Methods}{
    \subsection{Extracting values}{Data may be accessed as e.g.
        code{cm[["time"]]}, where the string could also be e.g.
        \code{"u"} or \code{"v"} for column data, or
        \code{"latitude"} or \code{"longitude"} for scalars.  (The names of the
        columns are displayed with \code{show().} The name of the source file
        is found with \code{"filename"}.}

    \subsection{Assigning values}{Everything that may be accessed may also be
        assigned, e.g.  \code{cm[["u"]] <- rep(35,100)}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(cm)}) displays information about the object.}
}

\seealso{A file containing CM profile data may be read with
  \code{\link{read.cm}}.  Statistical summaries are provided with
  \code{\link{summary.cm}}, while overviews are provided by \code{show}.
  Plots are provided with \code{\link{plot.cm}}.}
 
\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


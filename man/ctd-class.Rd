\name{ctd-class}

\docType{class}

\alias{ctd-class}
\alias{[[,ctd-method}
\alias{[[<-,ctd-method}
\alias{show,ctd-method}

\title{Class to store hydrographic data, e.g. from a CTD}

\description{Class to store hydrographic data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{
    \subsection{Extracting values}{Data may be accessed as e.g.
        code{ctd[["salinity"]]}, where the string could also be e.g.
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

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


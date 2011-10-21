\name{coastline-class}

\docType{class}

\alias{coastline-class}
\alias{[[,coastline-method}
\alias{[[<-,coastline-method}
\alias{show,coastline-method}

\title{Class to store coastline data}

\description{Class to store coastline data, with standard slots \code{metadata}
    (containing \code{fillable} and \code{filename}),  \code{data} (containing
    \code{latitude} and \code{longitude}) and \code{processingLog}.}

\section{Methods}{
    \subsection{Extracting values}{Data may be accessed as e.g.
        \code{ctd[["latitude"]]} or
        \code{ctd[["longitude"]]}.}

    \subsection{Assigning values}{Latitude may be changed with e.g.
        \code{ctd[["latitude"]] <- value}, and of course the same can be done
        for \code{longitude}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(coastline)}) displays information about the object.}
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


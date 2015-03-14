\name{coastline-class}

\docType{class}

\alias{coastline-class}
\alias{[[,coastline-method}
\alias{[[<-,coastline-method}
\alias{show,coastline-method}

\title{Class to store coastline data}

\description{Class to store coastline data, with standard slots \code{metadata}
    (containing \code{fillable} and \code{filename}),  \code{data} (containing
    \code{longitude} and \code{latitude}) and \code{processingLog}.}

\section{Methods}{

    \subsection{Extracting values}{Positional data may be accessed as e.g.
        \code{coastline[["longitude"]]} or \code{coastline[["latitude"]]}, with
        abbreviations (e.g. \code{coastline[["lo"]]}) being permitted so long
        as they are distinct. However, metadata must be named in full.}

    \subsection{Assigning values}{Latitude may be changed with e.g.
        \code{coastline[["longitude"]] <- value}, and of course the same can be
        done for \code{latitude}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(coastline)}) displays information about the object.}
}

\seealso{Use \code{\link{as.coastline}} to convert data to this form,
    \code{\link{read.coastline}} to read data in various formats, and
    \code{\link{plot.coastline}} to plot coastlines.}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


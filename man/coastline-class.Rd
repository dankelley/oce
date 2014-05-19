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

    \subsection{Extracting values}{Data may be accessed as e.g.
        \code{coastline[["longitude"]]} or \code{coastline[["latitude"]]}.}

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


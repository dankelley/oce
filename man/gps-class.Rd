\name{gps-class}

\docType{class}

\alias{gps-class}
\alias{[[,gps-method}
\alias{[[<-,gps-method}
\alias{show,gps-method}

\title{Class to store gps data}

\description{Class to store gps data, with standard slots \code{metadata}
    (\code{filename}), \code{data} (a list containing \code{longitude} and
    \code{latitude}) and \code{processingLog}.}

\section{Methods}{

    \subsection{Extracting values}{Data may be accessed as e.g.
        \code{gps[["longitude"]]} or \code{gps[["latitude"]]}.  Items in
        \code{metadata} must be specifield by full name, but those in
        \code{data} may be abbreviated, so long as the abbreviation is unique.}

    \subsection{Assigning values}{Latitude may be changed with e.g.
        \code{gps[["longitude"]] <- value}, and of course the same can be done
        for \code{latitude}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(gps)}) displays information about the object.} }

\seealso{Use \code{\link{as.gps}} to convert data to this form,
    \code{\link{read.gps}} to read GPX-format data, and \code{\link{plot.gps}}
    to plot data.}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


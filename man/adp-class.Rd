\name{adp-class}

\docType{class}

\alias{adp-class}
\alias{[[,adp-method}

\title{Class to store acoustic Doppler profiler data}

\description{Class to store acoustic Doppler proviler data, with standard
    slots \code{metadata}, \code{data} and \code{processingLog}.}


\section{Methods}{

    \subsection{Extracting values}{Data may be accessed as e.g.
        \code{adv[["u1"]]}, where the string could also be \code{"u2"}, etc.,
        for the other velocity components.  Use \code{"heading"},
        \code{"pitch"} or \code{"roll"} for the instrument angles.  Use
        \code{"temperature"} or \code{"pressure"} for pressure, or
        \code{"time"} for time.  Location data may be accessed with
        \code{adp[["latitude"]]} and \code{adp[["longitude"]]}, which return
        scalars.}

    \subsection{Assigning values}{(not available yet)}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(d)}) displays information about an ADV object named
        \code{d}.}
}

\seealso{A file containing ADP data is usually recognized by Oce, and so
    \code{\link{read.oce}} will usually read the data.  If not, one may use the
    general ADP function \code{\link{read.adp}} or specialized variants
    \code{\link{read.adp.rdi}}, \code{\link{read.adp.nortek}} or
    \code{\link{read.adp.sontek}} or
    \code{\link{read.adp.sontek.serial}}.

    ADV data may be plotted with \code{\link{plot.adp}} function, which is a
    generic function so it may be called simply as \code{plot}.

    Statistical summaries of ADV data are provided by the generic function
    \code{summary}, while briefer overviews are provided with \code{show}.

    Conversion from beam to xyz coordinates may be done with
    \code{\link{beamToXyzAdp}}, and from xyz to enu (east north up) may be done
    with \code{\link{xyzToEnuAdp}}.  \code{\link{toEnuAdp}} may be used to
    transfer either beam or xyz to enu.  Enu may be converted to other
    coordinates (e.g. aligned with a coastline) with
    \code{\link{enuToOtherAdp}}.}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


\name{adv-class}

\docType{class}

\alias{adv-class}
\alias{[[,adv-method}
\alias{[[<-,adv-method}
\alias{show,adv-method}

\title{Class to store acoustic Doppler velocimeter data}

\description{Class to store acoustic Doppler velocimeter data, holding the
    three standard slots used in all objects in Oce.  The \code{metadata} slot
    contains various items relating to the dataset, including source file name,
    sampling rate, velocity resolution and scale, etc.  The
    \code{processingLog} is in standard form and needs little comment.  The
    \code{data} slot holds a numeric matrix \code{v} of velocities in m/s, with
    the first index indicating time and the second indicating beam number.  The
    meanings of the beams depends on whether the object is in beam
    coordinates, frame coordinates, or earth coordinates.  The \code{data} slot
    also contains identically-dimensioned raw matrices \code{a} and \code{q},
    holding measures of signal strength and data quality quality, respectively.
    It also contains a series of vectors, e.g. \code{time}, \code{temperature}
    and \code{pressure}, etc., depending on what sensors are included in the
    package.  For all of these quantities, the details can be different for
    different instrument types, and it is assumed that the user will be
    familiar with the details.}

\section{Methods}{

    \subsection{Extracting values}{Data may be accessed as e.g.  for an object
        named \code{d}, the velocity matrix is retrieved by \code{d[["v"]]},
        the amplitude matrix by \code{d[["a"]]}, the data-quality matrix by
        \code{d[["q"]]}, etc.  (The last two can be retrieved in numerical
        form, as opposed to raw form, by e.g.  \code{d[["a", "numeric"]]}.)
        Items in \code{metadata} must be specifield by full name, but those in
        \code{data} may be abbreviated, so long as the abbreviation is unique.}

    \subsection{Assigning values}{This follows the standard form, e.g. to
        increase all velocity data by 1 cm/s, use \code{d[["v"]] <- 0.01 +
            d[["v"]]}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(d)}) displays information about an ADV object named
        \code{d}.}
}

\seealso{A file containing ADV data is usually recognized by Oce, and so
    \code{\link{read.oce}} will usually read the data.  If not, one may use the
    general ADV function \code{\link{read.adv}} or specialized variants
    \code{\link{read.adv.nortek}}, \code{\link{read.adv.sontek.adr}} or
    \code{\link{read.adv.sontek.text}}.

    ADV data may be plotted with \code{\link{plot.adv}} function, which is a
    generic function so it may be called simply as \code{plot}.

    Statistical summaries of ADV data are provided by the generic function
    \code{summary}, while briefer overviews are provided with \code{show}.

    Conversion from beam to xyz coordinates may be done with
    \code{\link{beamToXyzAdv}}, and from xyz to enu (east north up) may be done
    with \code{\link{xyzToEnuAdv}}.  \code{\link{toEnuAdv}} may be used to
    transfer either beam or xyz to enu.  Enu may be converted to other
    coordinates (e.g. aligned with a coastline) with
    \code{\link{enuToOtherAdv}}.
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


\name{adp-class}

\docType{class}

\alias{adp-class}
\alias{[[,adp-method}
\alias{[[<-,adp-method}
\alias{show,adp-method}

\title{Class to store acoustic Doppler profiler data}

\description{Class to store acoustic Doppler profiler data, holding the three
    slots used in all objects in Oce.  
    
    The \code{processingLog} is in standard form and needs little comment.  

    The \code{metadata} slot contains various items relating to the dataset,
    including source file name, sampling rate, velocity resolution and scale,
    etc.; some of these will be unique to a given instrument type.  Perhaps the
    most useful general properties (for an object named \code{d}, say) are
    \code{d[["bin1Distance"]]} (the distance, in metres, from the sensor to the
    bottom of the first bin), \code{d[["cellSize"]]} (the cell height, in
    metres, in the vertical direction, \emph{not} along the beam), and
    \code{d[["beamAngle"]]} (the angle, in degrees, between beams and an
    imaginary centre line that bisects all beam pairs).  
    
    The geometry of a four-beam profiler is illustrated below, for
    \code{d[["beamAngle"]]} equal to 20 degrees, \code{d[["bin1Distance"]]}
    equal to 2m, and \code{d[["cellSize"]]} equal to 0.5m.   (In the diagram,
    the viewer is in the plane containing two beams that are not shown, and the
    two visible beams are separated by 40 degrees.)
    
    \if{html}{\figure{adpgeometry.png}{options: width=400px
            alt="Figure: adpgeometry.png"}}
    
    The \code{data} slot holds a variety of items.  Again, assuming an object
    named \code{d}, \code{d[["v"]]} is a three-dimensional numeric matrix of
    velocities in m/s.  In this matrix, the first index indicates time, the
    second bin number, and the third beam number.  The meanings of the beams
    depends on whether the object is in beam coordinates, frame coordinates, or
    earth coordinates.  Corresponding to this are two matrices of type raw, and
    identical dimension, accessed by \code{d[["a"]]} and \code{d[["q"]]},
    holding measures of signal strength and data quality quality, respectively.

    In addition to the matrices, there are time-based vectors.  The vector
    \code{d[["time"]]} (of length equal to the first index of \code{d[["v"]]},
    etc.) holds POSIXt times of observation.  Depending on the configuration of
    the instrument, there may also be corresponding vectors for pressure
    (\code{d[["pressure"]]}) or temperature (\code{d[["temperature"]]}),
    depending on the setup of the instrument.

    Finally, there is a vector \code{d[["distance"]]} that indicates the bin
    distances from the sensor, meaasured in metres along an imaginary centre
    line bisecting beam pairs.  The length of this vector equals the second
    dimension of \code{d[["v"]]}.

}

\section{Methods}{

    \subsection{Extracting values}{Matrix data may be accessed as e.g.  for an
        adv object named \code{d}, the data are provided by \code{d[["v"]]},
        \code{d[["a"]]}, and \code{d[["q"]]}.  As a convenience, the last two
        of these can be accessed as numeric (as opposed to raw) values by e.g.
        \code{d[["a", "numeric"]]}.  The vectors are accessed in a similar way,
        e.g. \code{d[["heading"]]}, etc.  Quantities in the \code{metadata}
        slot are also available by name, e.g. \code{d[["velocityResolution"]]},
        etc.}

    \subsection{Assigning values}{This follows the standard form, e.g. to
        increase all velocity data by 1 cm/s, use \code{d[["v"]] <- 0.01 +
            d[["v"]]}.}

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
    \code{\link{enuToOtherAdp}}.
}

\author{Dan Kelley}
\keyword{classes}
\keyword{oce}


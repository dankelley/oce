\name{landsat-class}

\docType{class}

\alias{landsat-class}
\alias{[[,landsat-method}
\alias{[[<-,landsat-method}
\alias{show,landsat-method}

\title{Class to store landsat data}

\description{Class to store landsat data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.}

\section{Methods}{

    Consider a landsat object named \code{landsat}.

    \emph{Accessing data values.} The data may be accessed with e.g.
    \code{landsat[["band", 8]]}, for band number 8, or e.g.
    \code{landsat[["time"]]} for the time at which the image was acquired.
    
    \emph{Accessing metadata.} Anything in the metadata can be accessed by
    name, e.g. \code{landsat[["time"]]}.  Note that some items are simply
    copied over from the source data file and are not altered by e.g.
    decimation.  An exception is the lat-lon box, which is altered by
    \code{\link{landsatTrim}}.

    \emph{Overview of contents.} The \code{summary} method displays information
    about the object.

}

\seealso{A file containing Landsat data may be read with
    \code{\link{read.landsat}} or \code{\link{read.oce}}, and one such file is
    provided by the \code{ocedata} package as a dataset named \code{landsat}.

    Plots may be made with \code{\link{plot.landsat}}.  Since plotting can be
    quite slow, decimation is available both in the plotting function and as
    the separate function \code{link{decimate}}.  Images may be subsetted with
    \code{\link{landsatTrim}}.

    Landsat data are available at several websites; see e.g. [1].

    See [2] for information on the bands for Landsat 8, paying particular
    attention to the resolutions and wavelengths.  Band 8 is panchromatic, and
    has the highest resolution, so this is the default used in
    \code{\link{plot.landsat}}.}


\references{
1. \url{http://glovis.usgs.gov} 

2. \url{http://landsat.gsfc.nasa.gov/?page_id=5377}
}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


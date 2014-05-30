\name{landsat-class}

\docType{class}

\alias{landsat-class}
\alias{[[,landsat-method}
\alias{[[<-,landsat-method}
\alias{show,landsat-method}

\title{Class to store landsat data}

\description{Class to store landsat data, with standard slots
    \code{metadata}, \code{data} and \code{processingLog}.

    Landsat data are available at several websites; see e.g. Reference [1].

    Band information is as follows
\preformatted{
.------------------------------------------------------------------------------.
| Band | Band                      | Band         | Wavelength    | Resolution |
| No.  | Contents                  | Name         | (micrometers) |   (meters) |
|------+---------------------------+--------------+---------------+------------|
|    1 | Coastal aerosol           | aerosol      |  0.43 -  0.45 |         30 |
|    2 | Blue                      | blue         |  0.45 -  0.51 |         30 |
|    3 | Green                     | green        |  0.53 -  0.59 |         30 |
|    4 | Red                       | red          |  0.64 -  0.67 |         30 |
|    5 | Near Infrared (NIR)       | nir          |  0.85 -  0.88 |         30 |
|    6 | SWIR 1                    | swir1        |  1.57 -  1.65 |         30 |
|    7 | SWIR 2                    | swir2        |  2.11 -  2.29 |         30 |
|    8 | Panchromatic              | panchromatic |  0.50 -  0.68 |         15 |
|    9 | Cirrus                    | cirrus       |  1.36 -  1.38 |         30 |
|   10 | Thermal Infrared (TIRS) 1 | tirs1        | 10.60 - 11.19 |        100 |
|   11 | Thermal Infrared (TIRS) 2 | tirs2        | 11.50 - 12.51 |        100 |
.------------------------------------------------------------------------------.
}

    See Reference [2] for information on the bands for Landsat 8, paying
    particular attention to the resolutions and wavelengths.  Band 8 is
    panchromatic, and has the highest resolution, so this is the default used
    in \code{\link{plot.landsat}}. Note that \code{tirs1} and \code{tirs2} are
    subsampled to 30m in the data stream, but the true resolution is as stated
    above.
}

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
    the separate function \code{\link{decimate}}.  Images may be subsetted with
    \code{\link{landsatTrim}}.
}

\references{
1. \url{http://glovis.usgs.gov} 

2. \url{http://landsat.gsfc.nasa.gov/?page_id=5377}
}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


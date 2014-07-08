\name{read.landsat}

\alias{read.landsat}

\title{Read a landsat data file}

\description{Read a landsat data file, producing an object of type \code{landsat}.}

\usage{
read.landsat(file, band=1:11, debug=getOption("oceDebug"))
}

\arguments{

  \item{file}{a connection or a character string giving the name of the file to
      load.  This is a directory name containing the data.}

  \item{band}{the bands to read, by default all of the Landsat-8 bands.  See
      \sQuote{Value} for the names of the bands.}
  
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
      amount of debugging information, or to 2 to get more.} }

\details{The \code{tiff} package must be installed for this to work.
    
    The function follows a strict file naming code that works for Landsat-8
    datasets but may fail otherwise.  Also, it only works for detailed data (in
    TIF format), not for preview data (in JPG format).
    
    The bands are stored in the object with names "aerosol" (band 1), "blue"
    (band 2), "green" (band 3), "red" (band 4), "nir" (band 5), "swir1" (band
    6), "swir2" (band 7), "panchromatic" (band 8), "cirrus" (band 9), "tirs1"
    (band 10), and "tirs2" (band 11).

    See \code{\link{landsat-class}} for more information on the bands, and also
    for references on Landsat data sources and processing.}

\section{Storage requirements}{Landsat images are large, e.g. a panchromatic
    image has 16000 pixels on a side, and since the data are stored in a matrix
    with 8 bytes per entry, this works out to 2Gb of storage.  (The other bands
    require 1/4 this storage.)  Working with images this large demands a
    computer of 8Gb or more, because most operations will involve copying the
    matrix, which overhelms the memory capacity of a 4Gb machine.  As of summer
    2014, 8Gb of memory is about a hundred dollars, so upgrading may be an
    option in most science laboratories.}

\value{An object of class \code{landsat}, with the conventional Oce slots
    \code{metadata}, \code{data} and \code{processingLog}.  The \code{metadata}
    is mainly intended for use by Oce functions, but for generality it also
    contains an entry named \code{header} that represents the full image header
    in a list (with names made lower-case).  The \code{data} slot holds
    matrices of the data in the requested bands, and users may add extra
    matrices if desired, e.g. to store calculated quantities.} 

\seealso{See \code{\link{landsat-class}} for more information on \code{landsat}
    objects.  A sample dataset named \code{landsat} is available in the
    \code{ocedata} package.}

\author{Dan Kelley}

\keyword{misc}


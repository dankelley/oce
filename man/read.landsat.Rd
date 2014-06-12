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
    
    Landsat data are provided on several websites, e.g. [1].  Reference [2]
    provides information on the bands.

    The bands are stored in the object with names "aerosol" (band 1), "blue"
    (band 2), "green" (band 3), "red" (band 4), "nir" (band 5), "swir1" (band
    6), "swir2" (band 7), "panchromatic" (band 8), "cirrus" (band 9), "tirs1"
    (band 10), and "tirs2" (band 11).  See [2] for information on these bands
    and their typical uses.}

\value{An object of class \code{landsat}, with the conventional Oce slots
    \code{metadata}, \code{data} and \code{processingLog}.  The \code{metadata}
    is mainly intended for use by Oce functions, but for generality it also
    contains an entry named \code{header} that represents the full image header
    in a list (with names made lower-case).  The \code{data} slot holds
    matrices of the data in the requested bands, and users may add extra
    matrices if desired, e.g. to store calculated quantities.} 

\references{
1. \url{http://glovis.usgs.gov} 

2. \url{http://landsat.gsfc.nasa.gov/?page_id=5377}
}

\seealso{See \code{\link{landsat-class}} for more information on \code{landsat}
    objects.  A sample dataset named \code{landsat} is available in the
    \code{ocedata} package.}

\author{Dan Kelley}

\keyword{misc}


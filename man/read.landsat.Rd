\name{read.landsat}

\alias{read.landsat}

\title{Read a landsat data file}

\description{Read a landsat data file, producing an object of type \code{landsat}.}

\usage{
read.landsat(file, band="all", emissivity=0.984, debug=getOption("oceDebug"))
}

\arguments{

  \item{file}{a connection or a character string giving the name of the file to
      load.  This is a directory name containing the data.}

  \item{band}{the bands to read, by default all of the bands.  See
      \sQuote{Details} for the names of the bands.}
  
  \item{emissivity}{emissivity of the surface, stored as
      \code{metadata@emissivity} in the resultant object. This is used in the
      calculation of surface temperature, as explained in the discussion of
      accessor functions for \code{\link{landsat-class}}. The default value is
      from Konda et al.  (1994). These authors suggest an uncertainty of 0.04,
      but a wider range of values can be found in the literature.  The value of
      \code{metadata$emissivity} is easy to alter, either as a single value or
      as a matrix, yielding flexibility of calcuation.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
      amount of debugging information, or to 2 to get more.}
}

\details{The \code{tiff} package must be installed for this to work.
    
    Landsat data are provided in directories that contain TIFF files and header
    information, and \code{read.landsat} relies on a strict convention for the
    names of the files in those directories.  Those file names were found by
    inspection of some data, on the assumption that similar patterns will hold
    for other datasets for any given satellite. This is a brittle approach and
    it should be born in mind if \code{read.landsat} fails for a given dataset.
    
    For Landsat 8, there are 11 bands, with names \code{"aerosol"} (band 1),
    \code{"blue"} (band 2), \code{"green"} (band 3), \code{"red"} (band 4),
    \code{"nir"} (band 5), \code{"swir1"} (band 6), \code{"swir2"} (band 7),
    \code{"panchromatic"} (band 8), \code{"cirrus"} (band 9), \code{"tirs1"}
    (band 10), and \code{"tirs2"} (band 11).

    For Landsat 7, there 8 bands, with names \code{"blue"} (band 1),
    \code{"green"} (band 2), \code{"red"} (band 3), \code{"nir"} (band 4),
    \code{"swir1"} (band 5), \code{"tir1"} (band 6A), \code{"tir2"} (band 6B),
    \code{"swir2"} (band 7) and \code{"panchromatic"} (band 8).  
    
    For Landsat 4 and 5, the bands similar to Landsat 7 but without
    \code{"panchromatic"} (band 8).

    See \code{\link{landsat-class}} for more information on the data storage
    scheme, and also on the bands and their processing.}

\section{Storage requirements}{Landsat images are large, with the given scene
    requiring about a gigabyte of storage, adding the full suite of bands.  The
    storage of the Oce object is similar (see \code{\link{landsat-class}}).  In
    R, many operations involving copying data, so that dealing with full-scale
    landsat images can overwhelm computers with storage under 8GB.
    For this reason, it is typical to read just the bands that are of interest.
    It is also helpful to use \code{\link{landsatTrim}} to trim the data to a
    geographical range.}
   
\value{An object of class \code{landsat}, with the conventional Oce slots
    \code{metadata}, \code{data} and \code{processingLog}.  The \code{metadata}
    is mainly intended for use by Oce functions, but for generality it also
    contains an entry named \code{header} that represents the full image header
    in a list (with names made lower-case).  The \code{data} slot holds
    matrices of the data in the requested bands, and users may add extra
    matrices if desired, e.g. to store calculated quantities.} 

\seealso{See \code{\link{landsat-class}} for more information on \code{landsat}
    objects, especially band information.  Use \code{\link{landsatTrim}} to
    trim Landsat objects geographically and \code{\link{landsatAdd}} to add new
    ``bands.''  The accessor operator (\code{[[}) is used to access band
    information, full or decimated, and to access certain derived quantities.
    A sample dataset named \code{landsat} is available in the \CRANpkg{ocedata}
    package.}

\references{Konda, M. Imasato N., Nishi, K., and T. Toda, 1994.  Measurement of
    the Sea Surface Emissivity.  \emph{Journal of Oceanography}, 50, 17:30.
    Available at
    \url{http://www.terrapub.co.jp/journals/JO/pdf/5001/50010017.pdf} as of
    February 2015.}


\author{Dan Kelley}

\keyword{misc}


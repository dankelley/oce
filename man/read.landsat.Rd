\name{read.landsat}

\alias{read.landsat}

\title{Read a landsat data file}

\description{Read a landsat data file, producing an object of type \code{landsat}.}

\usage{
read.landsat(file, band, debug=getOption("oceDebug"))
}

\arguments{

  \item{file}{a connection or a character string giving the name of the file to
      load.  This is a directory name containing the data.}

  \item{band}{the band to read, e.g. 8 is panchromatic for Landsat-8.}
  
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
}

\details{This reads just one band, and follows a strict file naming code that
    works for Landsat-8 but may fail otherwise.}

\references{Landsat data are available from \url{http://glovis.usgs.gov} and
    other sources.}
 
\author{Dan Kelley}

\keyword{misc}


\name{read.echosounder}
\alias{read.echosounder}

\title{Read an echosounder data file}

\description{Read an echosounder data file, producing an object of type \code{echosounder}.}

\usage{
read.echosounder(file, debug=getOption("oceDebug"))
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
}

\details{Reads a biosonics echosounder file.}

\value{An object of \code{\link[base]{class}} \code{"echosounder"}, with
    standard slots as follows.
    
    \item{metadata}{a list containing the following items
        \describe{
            \item{\code{filename}}{name of the file passed to \code{read.echosounder}.}
        }
    }
    \item{data}{a list containing (FIXME: add content here)}

    \item{processingLog}{a processingLog of processing, in the standard \code{oce} format.}
}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of \code{ctd} objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
\dontrun{
e <- read.echosounder('data.dt4')
plot(e)
}
}

\references{
    Various echousounder instruments provided by BioSonics are described at the
    company website, \url{http://www.biosonicsinc.com/}.  The following document

    Biosonics, 2010.  DT4 Data File Format Specification.  BioSonics Advanced
    Digital Hydroacoustics. July, 2010.  SOFTWARE AND ENGINEERING LIBRARY
    REPORT BS&E-2004-07-0009-2.0.  (This document was provided to the author of
    this function in November 2011, which suggests that the data format was not
    changed since July 2010.)}

\author{Dan Kelley}

\keyword{misc}

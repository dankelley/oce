\name{read.argo}

\alias{read.argo}

\title{Read a argo data file}

\description{Read a argo file, producing an object of type \code{argo}.}

\usage{
read.argo(file, debug=getOption("oceDebug"), processingLog, \dots)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{\dots}{additional arguments, passed to called routines.}
}

\details{This reads netCDF formatted ARGO data.}

\value{An object of \code{\link[base]{class}} \code{"argo"}.}

\examples{
\dontrun{
library(oce)
d <- read.argo("/data/OAR/6900388_prof.nc")
summary(d)
plot(d)
}
}

\seealso{The documentation for \code{\link{argo-class}} explains the
    structure of argo objects, and also outlines the other functions dealing
    with them.}

\references{\url{http://www.argo.ucsd.edu/}}

\author{Dan Kelley}

\keyword{misc}

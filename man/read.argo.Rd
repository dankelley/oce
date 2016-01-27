% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{read.argo}

\alias{read.argo}

\title{Read an Argo data file}

\description{Read an Argo file, producing an object of type \code{argo}.}

\usage{
read.argo(file, debug=getOption("oceDebug"), processingLog, \dots)
}

\arguments{
  \item{file}{a character string giving the name of the file to load.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
    amount of debugging information, or to 2 to get more.} 

  \item{processingLog}{if provided, the action item to be stored in the log.
    (Typically only provided for internal calls; the default that it provides is
    better for normal calls by a user.)}

  \item{\dots}{additional arguments, passed to called routines.}
}

\details{This reads netCDF formatted ARGO data, as described at [2]}

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

\references{
1. \url{http://www.argo.ucsd.edu/}

2. \url{http://archimer.ifremer.fr/doc/00187/29825/40575.pdf} documents the codes used in the netCDF files.}

\author{Dan Kelley}

\keyword{misc}

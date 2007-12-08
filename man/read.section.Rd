\name{read.section}
\alias{read.section}
\title{Read a section containing multiple CTD profiles}
\description{Read a file that contains a series of CTD profiles that make up an oceanographic section.}
\usage{read.section(file, debug=FALSE)}
\arguments{
  \item{file}{Either a CTD object, in which case the rest of the arguments are other CTD objects, or a list of ctd objects.}
  \item{debug}{Set TRUE to print some information that might be helpful during debugging.}
}
\details{
  \emph{Caution.} This is still in development, and the syntax may change without notice.  Only ``ewoce'' comma-separated value format is permitted at this time, but other formats may be added later.
}
\value{
  A \code{section} object.  
}
\seealso{
  \code{\link{make.section}} can be used to manually bind together CTD objects into a section.
}
\examples{
\dontrun{
library(oce)
# http://cchdo.ucsd.edu/data_access?ExpoCode=90CT40_1
# ExpoCode 90CT40_1, Chief Scientist Tereschenkov/SOI
s <- read.section("a03_hy1.csv") 
plot(s)
summary(s)
}
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

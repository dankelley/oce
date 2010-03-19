\name{make.section}
\alias{make.section}
\alias{+.section}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}

\usage{
make.section(item, \dots)

\special{section + station}
}

\arguments{
  \item{item}{either a \code{ctd} object, in which case the rest of the
    arguments are other \code{ctd} objects, or a list of \code{ctd}
    objects.} 
  \item{\dots}{a list of \code{ctd} objects, either given as separate
    arguments (the normal procedure) or as a list.}
  \item{section}{a section to which a station is to be added.}
  \item{station}{a station to be added to a section.}
}

\details{The \code{ctd} stations must share identical pressure values;
  use \code{\link{section.grid}} to do that.}

\value{An object of \code{\link[base]{class}} "\code{section}" (for
  details, see \code{\link{read.section}}).}

\seealso{\code{\link{read.ctd}} reads CTD data.  \code{\link{section}}
  is a sample data set.}

\examples{
library(oce)
data(ctd)
ctd.warmed <- ctd
ctd.warmed$data$temperature <- ctd.warmed$data$temperature + 0.5
ctd.warmed$metadata$latitude <- ctd.warmed$metadata$latitude + 0.1
section <- make.section(ctd, ctd.warmed)
summary(section)

s2 <- make.section(ctd)
s2 <- s2 + ctd.warmed
plot(s2)

}
\author{Dan Kelley}
\keyword{misc}

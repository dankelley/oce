\name{make.section}
\alias{make.section}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}
\usage{make.section(\dots)}
\arguments{
  \item{\dots}{A list of CTD objects, either given as separate arguments (the normal procedure) or as a list.}
}
\details{
  This is still in development.  Decisions need to be made about whether
  the stations should be decimated to the same levels, whether they should
  be in some spatial or temporal order, etc.  
}
\value{
  A \code{section} object.  
}
\seealso{
  \code{\link{read.ctd}} reads CTD data.
}
\examples{
library(oce)
data(ctd)
ctd.warmed <- ctd
ctd.warmed$data$temperature <- ctd.warmed$data$temperature + 0.5
ctd.warmed$latitude <- ctd.warmed$latitude + 0.1
section <- make.section(ctd, ctd.warmed)
summary(section)
plot(section, at=c(1,2), labels=c("Original","Warmed"))
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}


\name{make.section.ctd}
\alias{make.section.ctd}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}
\usage{make.section(\dots)}
\arguments{
  \item{\dots}{The arguments name one or more CTD objects.}
}
\details{
  This is still in development.  Decisions need to be made about whether
  the stations should be decimated to the same levels, whether they should
  be in some spatial or temporal order, etc.  
}
\value{
  A \code{section} object.  
}
\seealso{
  \code{\link{read.ctd}} reads CTD data.
}
\examples{
library(oce)
data(ctd)
ctd.warmed <- ctd
ctd.warmed$data$temperature <- ctd.warmed$data$temperature + 0.5
ctd.warmed$latitude <- ctd.warmed$latitude + 0.1
section <- make.section(ctd, ctd.warmed)
summary(section)
plot(section, at=c(1,2), labels=c("Original","Warmed"))
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}


\name{make.section.list}
\alias{make.section.list}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}
\usage{make.section.list(\dots)}
\arguments{
  \item{\dots}{The arguments name one or more CTD objects. (FIXME).}
}
\details{
  This is still in development.  Decisions need to be made about whether
  the stations should be decimated to the same levels, whether they should
  be in some spatial or temporal order, etc.  
}
\value{
  A \code{section} object.  
}
\seealso{
  \code{\link{read.ctd}} reads CTD data.
}
\examples{
library(oce)
data(ctd)
ctd.warmed <- ctd
ctd.warmed$data$temperature <- ctd.warmed$data$temperature + 0.5
ctd.warmed$latitude <- ctd.warmed$latitude + 0.1
section <- make.section(ctd, ctd.warmed)
summary(section)
plot(section, at=c(1,2), labels=c("Original","Warmed"))
}
\references{
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}



\name{make.section}
\alias{make.section}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}
\usage{make.section(item, \dots)}
\arguments{
  \item{item}{Either a CTD object, in which case the rest of the arguments are other CTD objects, or a list of ctd objects.}
  \item{\dots}{A list of CTD objects, either given as separate arguments (the normal procedure) or as a list.}
}
\details{
  \emph{Caution.} This is still in development, and the syntax may change without notice.
  Decisions need to be made about whether
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

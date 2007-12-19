\name{make.section}
\alias{make.section}
\title{Bind CTD profiles together into a cross section}
\description{Combine a series of CTD profiles together to create a section.}
\usage{make.section(item, \dots)}
\arguments{
  \item{item}{either a \code{ctd} object, in which case the rest of the arguments are other \code{ctd} objects, or a list of \code{ctd} objects.}
  \item{\dots}{a list of \code{ctd} objects, either given as separate arguments (the normal procedure) or as a list.}
}

\details{The \code{ctd} stations must share identical pressure values; use
\code{\link{section.grid}} to do that.}

\value{A \code{section} object, as described at \code{\link{read.section}}.}

\seealso{\code{\link{read.ctd}} reads CTD data.}

\examples{
library(oce)
data(ctd)
ctd.warmed <- ctd
ctd.warmed$data$temperature <- ctd.warmed$data$temperature + 0.5
ctd.warmed$metadata$latitude <- ctd.warmed$metadata$latitude + 0.1
section <- make.section(ctd, ctd.warmed)
summary(section)
plot(section, at=c(1,2), labels=c("Original","Warmed"))
}
\author{Dan Kelley}
\keyword{misc}

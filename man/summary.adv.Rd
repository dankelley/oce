\name{summary.adv}

\alias{summary.adv}
\alias{print.summary.adv}

\title{Summarize an adv object}

\description{Summarize data in an \code{adv} object.}

\usage{
\method{summary}{adv}(object, \dots)
\method{print}{summary.adv}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"adv"}, usually, a result of a
    call to \code{\link{read.adv}}.}
  \item{x}{an object of class \code{"summary.adv"}, usually, a result of a
    call to \code{summary.adv}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.adv"}, which contains
  pertinent information about the ADV record and its processing.}

\seealso{A \code{adv} object may be read with \code{\link{read.adv}}.}

\examples{
\dontrun{
library(oce)
adv <- read.adv("~/data/sleiwex-archive/2008/mooring/m6/autopod/day183/SL08A183.015", 
       sampling.start=as.POSIXct("2008-07-01 15:00:00", tz="UTC"), deltat=0.1)
summary(adv)
}
}

\author{Dan Kelley}

\keyword{misc}

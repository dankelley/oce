\name{summary.lobo}

\alias{summary.lobo}

\alias{print.summary.lobo}

\title{Summarize a lobo data object}

\description{Summarizes some of the data in a lobo object.}

\usage{
\method{summary}{lobo}(object, \dots)

\method{print}{summary.lobo}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"lobo"}, usually, a result of a
    call to \code{\link{read.lobo}} or \code{\link{read.oce}}.}
  \item{x}{an object of class \code{"summary.lobo"}, usually, a result of a
    call to \code{summary.lobo}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the
  sampling interval, data ranges, etc.}

\value{An object of class \code{"summary.lobo"}, which contains
  pertinent information about the Lobo record and its processing.}

\seealso{The \code{lobo} object may be read with \code{\link{read.ctd}}.}

\examples{
\dontrun{
library(oce)
uri <- paste("http://loboviz.satlantic.com/cgi-bin/nph-data.cgi?",
	"min_date=20070110&max_date=20091230",
 	"&x=date&",
 	"y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
 	"data_format=text",sep="")
l <- read.lobo(uri)
summary(l)
}
}

\references{
\url{http://www.satlantic.com/default.asp?mn=1.15.27.139}
\url{http://www.mbari.org/lobo/}
}

\author{Dan Kelley}

\keyword{misc}

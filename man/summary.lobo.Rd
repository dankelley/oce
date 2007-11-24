\name{summary.lobo}
\alias{summary.lobo}
\title{Summarize a lobo data object}
\description{
  Summarizes some of the data in a lobo object.
}
\usage{
\method{summary}{lobo}(object, \dots)
}
\arguments{
  \item{object}{A \code{lobo} object, e.g. as read by \code{\link{read.lobo}}.}
  \item{\dots}{Passed to children.}
}
\details{
  Pertinent summary information is presented, including the sampling
  interval, data ranges, etc.
}
\value{
}

\seealso{
  The \code{lobo} object may be read with \code{\link{read.ctd}}.
}
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
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

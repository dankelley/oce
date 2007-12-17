\name{read.lobo}
\alias{read.lobo}
\title{Read a lobo data file}
\description{
  Read a data file created by a LOBO instrument.
}
\usage{
read.lobo(file, cols=7)
}
\arguments{
  \item{file}{A connection or a character string giving the name of the
    file to load.}
  \item{cols}{Number of columns in dataset.}
}
\details{

This version of read.lobo is really quite crude, having been developed mainly for a ``predict the
Spring bloom'' contest at Dalhousie University. In particular, the function assumes that the data
columns are exactly as specified in the Examples section; if you reorder the columns or add new
ones, this function is unlikely to work correctly. Furthermore, it should be noted that the file
format was inferred simply by downloading files; the supplier makes no claims that the format will
be fixed in time.

It is also worth noting that there is no \code{\link{read.oce}} equivalent to \code{read.lobo},
because the file format has no recognizable header.
}
\value{
  A \code{lobo} object containing:
\item{header}{The files' header.}
\item{log}{The processing log.}
\item{time}{Times of observations in the time series.}
\item{u}{Time series of u component of velocity.}
\item{v}{Time series of v component of velocity.}
\item{nitrate}{Time series of nitrate concentration.} 
\item{fluorescence}{Time series of fluorescence.}
\item{S}{Time series of salinity.}
\item{T}{Time series of temperature.}
\item{p}{Time series of pressure.}
\item{data}{The files's data, as a simple list.}
}

\note{The \code{oce} author was unable to find a description of the data format, and so
\code{read.lobo} makes some restrictive assumptions about the data format, expecting to find
exactly 7 columns, whose names in the header contain the words "date", "current across", "current
along", "nitrate", "fluorescence", "salinity", and "temperature". If these are not found,
\code{read.lobo} is likely to fail in some way. Luckily, the code is written in a simple way, so
that users should be able to alter it easily if, for example, the names of the current components
change to "current to the east", etc.
}
\seealso{
A \code{lobo} object may be summarized with
\code{\link{summary.lobo}} and plotted with
\code{\link{plot.lobo}}.
}
\examples{
\dontrun{
library(oce)
uri <- paste("http://loboviz.satlantic.com/cgi-bin/nph-data.cgi?",
	"min_date=20070220&max_date=20070305",
 	"&x=date&",
 	"y=current_across1,current_along1,nitrate,fluorescence,salinity,temperature&",
 	"data_format=text",sep="")
lobo <- read.lobo(uri)
summary(lobo)
}
}
\source{The file was created with the lines given in the example.}
\references{
\url{http://www.satlantic.com/default.asp?mn=1.15.27.139}
\url{http://www.mbari.org/lobo/}
}
\author{Dan Kelley}
\keyword{misc}

\name{ptr}
\alias{ptr}

\docType{data}

\title{ptr (pressure-temperature) dataset}

\description{A sample ptr file, created by 
\preformatted{
ptr <- read.oce("/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/manufacturer/pt_rbr_011855.dat", by=600)
patm <- ptPatm(ptr)[4]
ptr$data$pressure <- ptr$data$pressure - patm
ptr <- historyAppend(ptr, "subtract atmospheric pressure")
ptr <- window(ptr, start=as.POSIXct("2008-06-26",tz="UTC"), end=as.POSIXct("2008-06-27",tz="UTC"))
save(ptr, file="ptr.rda")
}
(which uses a file not supplied with this package).

Note that the name of this object is an exception to the usual in the
\code{oce} package, in which the name of the example matches the object
type.  This is because the name "\code{pt}" is already used for a
function relating to the "t" distribution.
}

\usage{data(ptr)}

\source{The \acronym{SLEIWEX} experiment.}

\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}

\seealso{See \code{\link{read.pt}} for reading such files, 
\code{\link{plot.pt}} for plotting them, 
\code{\link{summary.pt}} for summarizing them,
\code{\link{ptPatm}} for determining the atmospheric pressure,
and
\code{\link{ptTrim}} for a crude method of removing in-air samples.}

\examples{
library(oce)
data(ptr)
plot(ptr)
}
\author{Dan Kelley}
\keyword{misc}

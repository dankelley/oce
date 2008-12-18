\name{tdr}
\docType{data}
\alias{tdr}
\title{Sample temperature-depth recorder file}

\description{A sample RBR TDR file, created by 
\code{tdr <- read.oce("tests/tdr.dat")}
and then  \code{save(tdr, file="../data/tdr.rda")}.}
\usage{data(tdr)}
\source{The SLEIWEX experiment.}
\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}
\seealso{
See \code{\link{read.tdr}} for reading such files, 
\code{\link{plot.tdr}} for plotting them, 
\code{\link{summary.tdr}} for summarizing them,
\code{\link{tdr.patm}} for determining the atmospheric pressure,
and
\code{\link{tdr.trim}} for a crude method of removing in-air samples.}

\examples{
library(oce)
data(tdr)
plot(tdr)
tdr.trimmed <- tdr.trim(tdr)
plot(tdr.trimmed)
}
\author{Dan Kelley}
\keyword{misc}

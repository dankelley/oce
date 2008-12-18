\name{tdr}
\docType{data}
\alias{tdr}
\title{Sample temperature-depth recorder file}

\description{A sample RBR TDR file, created by 
\code{tdr <- read.oce("tests/tdr.dat")}
and then  \code{save(tdr, file="../data/tdr.rda")}.}
\examples{
data(tdr)
plot(tdr)
}
\usage{data(tdr)}
\source{The SLEIWEX experiment.}
\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}
\seealso{
See \code{\link{read.tdr.R}} for reading such files, 
\code{\link{plot.tdr.R}} for plotting them, 
\code{\link{summary.tdr.R}} for summarizing them,
\code{\link{tdr.patm.R}} for determining the atmospheric pressure,
and
\code{\link{tdr.trim.R}} for a crude method of removing in-air samples.}

\author{Dan Kelley}
\keyword{misc}

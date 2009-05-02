\name{pt}
\docType{data}
\alias{pt}
\title{Sample pressure-temperature recorder file}

\description{A sample pT file,
created by 
\code{pt <- read.oce("../tests/pt.dat")}
and then  
\code{save(pt, file="../data/pt.rda")}.}
\usage{data(pt)}
\source{The SLEIWEX experiment.}
\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}
\seealso{
See \code{\link{read.pt}} for reading such files, 
\code{\link{plot.pt}} for plotting them, 
\code{\link{summary.pt}} for summarizing them,
\code{\link{pt.patm}} for determining the atmospheric pressure,
and
\code{\link{pt.trim}} for a crude method of removing in-air samples.}

\examples{
library(oce)
data(pt)
plot(pt)
pt.trimmed <- pt.trim(pt)
plot(pt.trimmed)
}
\author{Dan Kelley}
\keyword{misc}

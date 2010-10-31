\name{pT}
\alias{pT}

\docType{data}

\title{pT (pressure-temperature) dataset}

\description{A sample pT file, created by 
\preformatted{
pT <- read.oce(
 "/data/archive/sleiwex/2008/moorings/m08/pt/rbr_011855/manufacturer/pt_rbr_011855.dat",
 by=600, from=as.POSIXct("2008-06-26",tz="UTC"),
 to=as.POSIXct("2008-06-27",tz="UTC"))
save(pT, file="pT.rda")
} (which uses a file not supplied with this package).
}

\usage{data(pT)}

\source{The \acronym{SLEIWEX} experiment.}

\references{\url{http://myweb.dal.ca/kelley/SLEIWEX/index.php}}

\seealso{See \code{\link{read.pt}} for reading such files, 
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

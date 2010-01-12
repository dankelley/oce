\name{pt}
\docType{data}
\alias{pt}
\title{pT (pressure-temperature) dataset}

\description{A sample pT file, created by 
\preformatted{
pt <- decimate(read.oce("sl08_011855.dat"), by=600)
save(pt, file="oce/data/pt.rda")
} (which uses a file not supplied with this package).
}

\usage{data(pt)}

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

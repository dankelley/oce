\name{format.with.ci}
\alias{format.with.ci}
\title{Format a confidence interval in parenthetic notation}
\description{Format a confidence interval in parenthetic notation}

\usage{format.with.ci(ci, style=c("+-", "parentheses")}

\arguments{
  \item{ci}{vector of length 2, giving lower and upper range of
    confidence interval}
  \item{style}{string indicating notation to be used.}
}

\details{PROVISIONAL CODE -- not tested.}

\value{Character string with the estimate and its uncertainty, in parenthetic notation.}

\examples{
cat("10 +- 2 is:", format.with.ci(c(8, 12)), "\n")
}

\author{Dan Kelley}

\references{http://en.wikipedia.org/wiki/Uncertainty}

\keyword{misc}

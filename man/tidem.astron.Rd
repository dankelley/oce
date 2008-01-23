\name{tidem.astron}
\alias{tidem.astron}
\title{Do ephemeris calculations for tidem}
\description{Do ephemeris calculations for tidem.}
\usage{a <- tidem.astron(t)}
\arguments{
  \item{t}{time in POSIXct format}
}
\details{Based directly on t\_astron, from the T\_TIDE package.}
\value{A \code{\link[base]{data.frame}} containing
  \item{\code{astro}}{(fill in later)}
  \item{\code{ader}}{(fill in later)}
}
\examples{
tidem.astron(as.POSIXct("2008-01-22 18:50:24"))
}
\seealso{
  \code{\link{tidem}}, which uses this.
}
\author{Dan Kelley, based directly on t\_astron from the T\_TIDE package.}
\keyword{misc}

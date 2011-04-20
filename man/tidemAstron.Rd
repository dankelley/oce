\name{tidemAstron}

\alias{tidemAstron}

\title{Do ephemeris calculations for tidem}

\description{Do ephemeris calculations for tidem.}

\usage{tidemAstron(t)}

\arguments{
  \item{t}{time in \code{POSIXct} format.  (It is
    \strong{very} important to use \code{tz="GMT"} in constructing
    \code{t}.)}
}

\details{Based directly on \code{t_astron}, from the \code{T_TIDE} package.}
\value{A \code{\link[base]{data.frame}} containing
  \item{\code{astro}}{(fill in later)}
  \item{\code{ader}}{(fill in later)}
}

\examples{tidemAstron(as.POSIXct("2008-01-22 18:50:24"))}

\seealso{\code{\link{tidem}}, which uses this.}

\author{Dan Kelley, based directly on \code{t_astron} from the \code{T_TIDE} package.}

\keyword{misc}

\concept{tide}

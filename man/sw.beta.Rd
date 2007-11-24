\name{sw.beta}
\alias{sw.beta}
\title{Seawater haline contraction coefficient}
\description{
Compute \eqn{\beta}{beta} using McDougall's (1987) algorithm. NOTE: this conflicts with base::beta, so
be sure to prefix as sw.beta().
}
\usage{
  b <- sw.beta(S, t, p, is.theta=FALSE);
}
\arguments{
  \item{S}{salinity [PSU]}
  \item{t}{Temperature or potential temperature [\eqn{^\circ}{deg}C], according to is.theta}
  \item{p}{pressure [dbar]}
  \item{is.theta}{Set TRUE if t is theta or FALSE if t is in-situ}
}
\details{
  McDougall (1987).  Note the use of potential temperature, not temperature.
}
\value{
  Value in 1/psu.
}

\seealso{
  N/A.
}
\examples{
 # 0.72088e-3 (from p1964 of McDougall 1987)
 b <- sw.beta(40, 10, 4000, is.theta=TRUE) 
}
\references{
McDougall, T.J. 1987. "Neutral Surfaces"
Journal of Physical Oceanography vol 17 pages 1950-1964
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

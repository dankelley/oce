\name{sw.T.freeze}
\alias{sw.T.freeze}
\title{Seawater freezing temperature}
\description{
  Compute freezing temperature of seawater.
}
\usage{
  T.freeze <- sw.T.freeze(S, p);
}
\arguments{
  \item{S}{\emph{in-situ} salinity [PSU]}
  \item{p}{\emph{in-situ} pressure [dbar]}
}
\details{
}
\value{
  Temperature [\eqn{^\circ}{deg}C]
}
\seealso{
  N/A.
}
\examples{
 T.freeze <-  sw.T.freeze(40, 500) # -2.588567 degC
}
\references{
UNESCO tech. papers in the marine science no. 28. 1978
eighth report JPOTS
Annex 6 freezing point of seawater F.J. Millero pp.29-35.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

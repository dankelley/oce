\name{sw.conductivity}
\alias{sw.conductivity}
\title{Seawater conductivity}
\description{
  Compute seawater conductivity, in \eqn{W m^{-1\circ}C^{-1}}{W/(m*degC)}
}
\usage{
  c <- sw.conductivity(S, t, p);
}
\arguments{
  \item{S}{salinity [PSU]}
  \item{t}{\emph{in-situ} temperature [\eqn{^\circ}{deg}C]}
  \item{p}{pressure [\eqn{db}{db}]}
}
\details{
 (\strong{To do:} Fill in a summary of Caldwell's technique.)
 \strong{Caution.} The results differ from Fofonoff's (1962)
 table 5 by 0.1 percent at 35PSU, and by under 1 percent 
 for fresh water.  (\strong{To do:} Compare this with Caldwell's stated uncertainty.)
}
\value{
  Conductivity of seawater in \eqn{W m^{-1\,\circ}C^{-1}}{W/(m*degC)}.  
  To calculate thermal diffusivity in \eqn{m^2/s}{m^2/s}, divide
  by the product of density and specific heat, as in the example.
}
\seealso{
  N/A.
}
\examples{
 library(oce)
 cond <- sw.conductivity(10,35,100); # 0.618569
 diffusivity <- cond / (sw.rho(10,35,100) * sw.specific.heat(10,35,100))
}
\references{
 Caldwell, Douglas R., 1974. Thermal conductivity of seawater,
 \emph{Deep-sea Research}, \bold{21}, 131-137.

 Fofonoff, N. P., 1962.  Physical properties of sea-water,
 \emph{The Sea}, \bold{1}, 3-30.
}     
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

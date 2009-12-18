\name{oce.approx}
\alias{oce.approx}
\title{Approximate with }
\description{Approximate.}
\usage{oce.approx(x, y, xout, n, m)}

\arguments{
  \item{x}{the independent variable (z or p, usually).}
  \item{y}{the dependent variable.}
  \item{xout}{the values of the independent variable at which
    interpolation is to be done.}
  \item{n}{NOT USED; eventually, will be as in Reiniger and Ross (1968).}
  \item{m}{NOT USED; eventually, will be as in Reiniger and Ross (1968).}
}

\details{FILL IN.}

\value{A vector of interplated values.}

\examples{
library(oce)
data(RRprofile)
ctd <- as.ctd(RRprofile$salinity, RRprofile$temperature, RRprofile$depth)
plot.profile(ctd, xtype="T")
Tinterp <- oce.approx(RRprofile$depth, RRprofile$temperature)
lines(Tinterp,  RRprofile$depth)
}

\references{R.F. Reiniger and C.K. Ross (1968). A method of
interpolation with application to oceanographic data. 
\emph{Deep Sea Research},
\bold{15}, 185-193.}

\author{Dan Kelley}

\keyword{misc}

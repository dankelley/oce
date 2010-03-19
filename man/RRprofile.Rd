\name{RRprofile}

\docType{data}

\alias{RRprofile}

\title{Seawater profile used by Reiniger and Ross (1968)}

\description{This is the hydrographic profile used by Reiniger and Ross
  (1968) to illustrate the use of their method for interpolating
  oceanographic data.}

\usage{data(RRprofile)}

\source{Table 2 of Reiniger and Ross (1968).}

\references{R.F. Reiniger and C.K. Ross, 1968.  A method of
interpolation with application to oceanographic data.  \emph{Deep Sea
Research}, \bold{15}, 185-193.}

\examples{
data(RRprofile)
ctd <- as.ctd(RRprofile$salinity, RRprofile$temperature, RRprofile$depth)
plot(ctd)
zz <- seq(0,2000,5)
plot(RRprofile$temperature, RRprofile$depth, ylim=c(500,0), xlim=c(2,11))
TT <- oce.approx(RRprofile$depth, RRprofile$temperature, zz)
lines(TT, zz)
}

\keyword{misc}

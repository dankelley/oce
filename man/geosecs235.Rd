\name{geosecs235}

\docType{data}

\alias{geosecs235}

\title{GEOSECS station 235 data}

\description{GEOSECS station 235 data}

\usage{data(geosecs235)}

\examples{
library(oce)
data(geosecs235)
plotProfile(geosecs235, xtype="tritium", type='p', ylim=c(600, 0))
trit <- geosecs235$data$tritium
p <- geosecs235$data$pressure
ok <- !is.na(p) & !is.na(trit)
trit <- trit[ok]
p <- p[ok]
#plot(trit, p, ylim=rev(range(p)))
m <- nls(trit~a*exp(-((p-p0)/D)^2)+a*exp(-((p+p0)/D)^2), start=list(a=10,p0=200,D=100))
pp <- seq(max(p), 0, length.out=100)
lines(predict(m, newdata=list(p=pp)), pp)
}

\source{Data from \url{http://iridl.ldeo.columbia.edu/SOURCES/.GEOSECS/ISTA/237/VALUE/data.cdf}}

\references{The curve fit shown in the example is as Figure 10 of D. E. Kelley
    and Van Scoy, K. A., 1999.  A basin-wide estimate of vertical mixing in the
    upper pycnocline: spreading of bomb tritium in the North Pacific Ocean.
   \emph{Journal of Physical Oceanography}, 29: 1759--1771.}

\keyword{datasets}

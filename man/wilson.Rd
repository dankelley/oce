\name{wilson}

\docType{data}

\alias{wilson}

\title{Wilson's (1963) table of seafloor spreading}

\description{Wilson's (1963) table of seafloor spreading.  This dataset is used in Chapter 4 of Kelley (2012).}


\usage{data(wilson)}

\examples{
library(oce)
data(wilson)
attach(wilson)
plot(Age, Distance, xlab="Age [My]", ylab="Distance [km]", xlim=c(125,0), ylim=c(0,4000))
abline(0,4000/125)
m <- lm(Distance~Age-1, data=wilson)
## Wilson reports in cm/year; 1cm/y = 1km/1My * 1e5 / 1e6
spreadingRate <- as.numeric(coef(m)) / 10 
print(spreadingRate)
abline(m, col='red')
detach(wilson)
}

\source{Data transcribed from Wilson's (1963) Table 1, taking only the
    first 3 columns, and only those rows for which Wilson reported an estimate
    of spreading rate.}

\references{
    Dan Kelley, in preparation. Oceanographic Analysis with R.  Springer Verlag.

    J. Tuzo Wilson, 1963. Evidence from islands on the spreading of ocean floors.  \emph{Nature}, 197(4867):536-538.  \url{//dx.doi.org/10.1038/197536a0.}
}

\keyword{datasets}

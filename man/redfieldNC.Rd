\name{redfieldNC}

\docType{data}

\alias{redfieldNC}

\title{Redfield's (1934) NO3 and total CO2 data}

\description{Data digitized from Redfield's (1934) Figure 3, showing NO$_3$
    dependence on total CO$_2$.  This dataset is used in Chapter 4 of Kelley
    (2011).}

\usage{data(redfieldNC)}

\examples{
data(redfieldNC)
attach(redfieldNC)
plot(CO2, NO3, xlab=expression(CO[2]), ylab=expression(NO[3]))
slope <- 1/7
abline(-275.5, slope, lwd=3)
abline(-288.5, slope, lwd=3)
}

\source{Data digitized by Dan Kelley from a scanned copy of Redfield's (1934).}

\seealso{See \code{\link{redfieldNP}} and \code{\link{redfieldPlankton}}.}

\references{Kelley, 2011.  Oceanographic Analysis with R.  Springer Verlag.  To appear.}

\keyword{misc}

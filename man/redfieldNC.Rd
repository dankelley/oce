\name{redfieldNC}

\docType{data}

\alias{redfieldNC}

\title{Redfield's (1934) NO3 and total CO2 data}

\description{Data digitized from Redfield's (1934) Figure 3, showing NO$_3$
    dependence on total CO$_2$.  This dataset is used in Chapter 4 of Kelley
    (2012).}

\usage{data(redfieldNC)}

\examples{
data(redfieldNC)
attach(redfieldNC)
plot(CO2, NO3, xlab=expression(CO[2]), ylab=expression(NO[3]))
slope <- 1/7
abline(-275.5, slope, lwd=3)
abline(-288.5, slope, lwd=3)
detach(redfieldNC)
}

\source{Data digitized by Dan Kelley from a scanned copy of Redfield (1934).}

\seealso{See \code{\link{redfieldNP}} and \code{\link{redfieldPlankton}}.}

\references{
    Dan Kelley, in preparation. Oceanographic Analysis with R.  Springer Verlag.

    Alfred C. Redfield, 1934.  On the proportions of organic derivations in sea
    water and their relation to the composition of plankton.  Pages 177--192 in
    James Johnstone Memorial Volume, University Press of Liverpool.
}

\keyword{datasets}

\name{redfieldNP}

\docType{data}

\alias{redfieldNP}

\title{Redfield's (1934) NO3 and PO4 data}

\description{Data digitized from Redfield's (1934) Figure 1, showing NO$_3$
    dependence on PO$_4$.  This dataset is used in Chapter 4 of Kelley (2011).}

\usage{data(redfieldNP)}

\examples{
data(redfieldNP)
attach(redfieldNP)
attach(redfieldNP)
plot(PO4, NO3, xlab=expression(PO[4]), ylab=expression(NO[3]))
abline(0, 20, lwd=3)                   # line N:P = 20:1, as shown by Redfield
}

\source{Data digitized by Dan Kelley from a scanned copy of Redfield's (1934)
    Figure 1, showing the dependence of NO$_3$ on PO$_4$.}

\references{Kelley, 2011.  Oceanographic Analysis with R.  Springer Verlag.  To appear.}

\seealso{See \code{\link{redfieldNC}} and \code{\link{redfieldPlankton}}.}

\keyword{misc}

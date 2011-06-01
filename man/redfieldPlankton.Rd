\name{redfieldPlankton}

\docType{data}

\alias{redfieldPlankton}

\title{Redfield's (1934) Table II}

\description{Contents of Redfield's (1934) Table II, containing columns for
    sampled species, carbon content by weight (normalized to 100), Nitrogen
    content by weight, and Phosphorus content by weight. This dataset is used in
    Chapter 4 of Kelley (2011).}

\usage{data(redfieldPlankton)}

\examples{
data(redfieldPlankton)
attach(redfieldPlankton)
par(mfrow=c(1,2))
boxplot(Nitrogen)
abline(h=16.7, col='gray')
boxplot(Phosphorus)
abline(h=1.85, col='gray')
}

\source{Data entered verbatim (sans italics for species name) by Dan Kelley
    from Redfield's (1934) Table II.}

\references{Kelley, 2011.  Oceanographic Analysis with R.  Springer Verlag.  To appear.}

\seealso{See \code{\link{redfieldNP}} and \code{\link{redfieldNC}}.}

\keyword{misc}

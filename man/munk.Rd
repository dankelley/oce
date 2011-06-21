\name{munk1966}

\docType{data}

\alias{munk1966}

\title{Munk's (1966) temperature profile}

\description{Temperature profile digitized from the left-hand panel of Munk's
    Figure 1.  This dataset is used in Chapter 3 of Kelley (2011).}

\usage{data(munk1966)}

\examples{
data(munk1966)
attach(munk1966)
plot(temperature, depth,
     ylim=rev(range(depth)), xlab="Temperature [degC]", ylab="Depth [km]")
detach(munk1966)
}

\references{
Dan Kelley, in preparation. Oceanographic Analysis with R.  Springer Verlag.

Walter H. Munk, 1966.  Abyssal recipes.  Deep-Sea Research, 13, 707-730.
}

\keyword{datasets}

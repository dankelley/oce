\name{munk}

\docType{data}

\alias{munk}

\title{Munk's (1966) temperature profile}

\description{Temperature profile digitized from the left-hand panel of Munk's
    Figure 1.  This dataset is used in Chapter 4 of Kelley (2012).}

\usage{data(munk)}

\examples{
data(munk)
attach(munk)
plot(temperature, depth,
     ylim=rev(range(depth)), xlab="Temperature [degC]", ylab="Depth [km]")
detach(munk)
}

\references{
Dan Kelley, in preparation. Oceanographic Analysis with R.  Springer Verlag.

Walter H. Munk, 1966.  Abyssal recipes.  Deep-Sea Research, 13, 707-730.
}

\keyword{datasets}

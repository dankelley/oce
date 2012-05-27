\name{plotTaylor}

\alias{plotTaylor}

\title{Plot a model-data comparison diagram.}

\description{Plot a model-data comparison diagram as described by Taylor
    (2001).}

\usage{plotTaylor(x, y, scale, pch, col)}

\arguments{
    \item{x}{a vector of predicted values of some quantity.}
    \item{y}{a vector or array of observed values of some quantity.
        If an array, then each column is taken to be set of observations.}
    \item{scale}{optional scale, interpreted as the maximum value of standard
        deviation}
    \item{pch}{list of characters to plot, one for each column of
        \code{y}}
    \item{col}{list of colours for points on the plot, one for each column of
        \code{y}}
}

\details{Creates a diagram as described by Taylor (2001).}

\references{Taylor, Karl E., 2001.  Summarizing multiple aspects of model
    performance in a single diagram, \emph{J. Geophys. Res.}, 106:D7,
    7183--7192.}

\examples{
library(oce)
data(sealevelHalifax)
x <- sealevelHalifax[["elevation"]]
y <- predict(tidem(sealevelHalifax))
yM2 <- predict(tidem(sealevelHalifax, constituents="M2"))
yS2 <- predict(tidem(sealevelHalifax, constituents="S2"))
plotTaylor(x, cbind(y, yM2, yS2))
}

\author{Dan Kelley}
\keyword{hplot}

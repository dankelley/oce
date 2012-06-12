\name{plotTaylor}

\alias{plotTaylor}

\title{Plot a model-data comparison diagram.}

\description{Plot a model-data comparison diagram as described by Taylor
    (2001).}

\usage{plotTaylor(x, y, scale, pch, col)}

\arguments{
    \item{x}{a vector of reference values of some quantity, e.g. measured over
        time or space.}
    \item{y}{a matrix whose columns hold values of values to be compared with
        those in x.  (If \code{y} is a vector, it is converted first to a
        one-column matrix).}
    \item{scale}{optional scale, interpreted as the maximum value of standard
        deviation.}
    \item{pch}{list of characters to plot, one for each column of
        \code{y}.}
    \item{col}{list of colours for points on the plot, one for each column of
        \code{y}.}
}

\details{Creates a diagram as described by Taylor (2001).  The graph is in the
    form of a semicircle, with radial lines and spokes connecting at a focus
    point on the flat (lower) edge.  The radius of a point on the graph
    indicates the standard deviation of the corresponding quantity, i.e. x and
    the columns in y.  The angle connecting a point on the graph to the focus
    provides an indication of correlation coefficient with respect to x.  The
    ``east'' side of the graph indicates \eqn{R=1}{R=1}, while \eqn{R=0}{R=0}
    is at the ``north edge'' and \eqn{R=-1}{R=-1} is at the ``west'' side.  The
    \code{x} data are indicated with a bullet on the graph, appearing on the
    lower edge to the right of the focus at a distance indicating the standard
    deviation of \code{x}.  The other points on the graph represent the columns
    of \code{y}, coded automatically or with the supplied values of \code{pch}
    and \code{col}.

    The example shows two tidal models of the Halifax sealevel data, computed
    with \code{\link{tidem}} with just the M2 component and the S2 component;
    the graph indicates that the M2 model is much better than the S2 model.}
    
\references{Taylor, Karl E., 2001.  Summarizing multiple aspects of model
    performance in a single diagram, \emph{J. Geophys. Res.}, 106:D7,
    7183--7192.}

\examples{
library(oce)
data(sealevelHalifax)
x <- sealevelHalifax[["elevation"]]
yM2 <- predict(tidem(sealevelHalifax, constituents="M2"))
yS2 <- predict(tidem(sealevelHalifax, constituents="S2"))
plotTaylor(x, cbind(yM2, yS2))
}

\author{Dan Kelley}
\keyword{hplot}

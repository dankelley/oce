\name{oce.plot.sticks}
\alias{oce.plot.sticks}

\title{Draw a stick-plot diagram}

\description{Draw a stick-plot diagram}

\usage{
oce.plot.sticks(x, y, u, v, yscale=1, add=FALSE, length=1/20,
                mgp=getOption("oceMgp"),
                mar=c(mgp[1]+1,mgp[1]+1,1,1+par("cex")),
                \dots)}
\arguments{
\item{x}{x coordinates of stick origins.}
\item{y}{y coordinates of stick origins.}
\item{u}{x coordinates of stick lengths.}
\item{v}{y coordinates of stick lengths.}
\item{yscale}{scale from u and v to y (see \dQuote{Details}).}
\item{add}{boolean, set \code{TRUE} to add to an existing plot.}
\item{length}{value to be provided to \code{\link{arrows}}; here, we set
  a default that is smaller than normally used, because these plots tend
  to be crowded in oceanographic applications.}
\item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
  also for \code{par(mar)}, computed from this.  The default is
  tighter than the R default, in order to use more space for the data
  and less for the axes.}
\item{mar}{value to be used with \code{\link{par}("mar")}.}
\item{\dots}{graphical parameters passed down to \code{\link{arrows}}.
  It is common, for example, to use smaller arrow heads than
  \code{\link{arrows}} uses; see \dQuote{Examples}.}
}

\details{The arrows are drawn with directions on the graph that match
  the directions indicated by the \code{u} and \code{v} components. The
  arrow size is set relative to the units of the \code{y} axis,
  according to the value of \code{yscale}, which has the unit of
  \code{v} divided by the unit of \code{y}.

  The interpretation of diagrams produced by \code{oce.plot.sticks} can
  be difficult, owing to overlap in the arrows.  For this reason, it It
  is often a good idea to smooth \code{u} and \code{v} before using this
  function.
}

\examples{
library(oce)

# Flow from a point source
n <- 16
x <- rep(0, n)
y <- rep(0, n)
theta <- seq(0, 2*pi, length.out=n)
u <- sin(theta)
v <- cos(theta)
oce.plot.sticks(x, y, u, v, xlim=c(-2, 2), ylim=c(-2, 2))
rm(n, x, y, theta, u, v)

# Oceanographic example
data(air.hal)
attach(air.hal)
oce.plot.ts(t, temperature, type='l', ylim=c(0,25), ylab=resizableLabel("T"))
oce.plot.sticks(t, rep(5, length(u)), u, v, yscale=2, add=TRUE)
legend("bottomright", legend="Velocity scale: 2 m/s per degC", bg="white")
detach(air.hal)
}

\author{Dan Kelley}

\keyword{misc}

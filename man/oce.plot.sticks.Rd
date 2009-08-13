\name{oce.plot.sticks}
\alias{oce.plot.sticks}

\title{Draw a stick-plot diagram}

\description{Draw a stick-plot diagram}

\usage{oce.plot.sticks(x, y, u, v, yscale=1, add=FALSE, \dots)}

\arguments{
\item{x}{x coordinates of stick origins.}
\item{y}{y coordinates of stick origins.}
\item{u}{x coordinates of stick lengths.}
\item{v}{y coordinates of stick lengths.}
\item{yscale}{scale from u and v to y (see \dQuote{Details}).}
\item{add}{boolean, set \code{TRUE} to add to an existing plot.}
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
data(air.hal)
attach(air.hal)
oce.plot.ts(t, temperature, type='l', ylim=c(-5,25), ylab=resizable.label("T"))
yscale <- 1
oce.plot.sticks(t, rep(5, length(u)), u, v, yscale=yscale, add=TRUE, length=1/20)
legend("bottomright", legend=sprintf("Velocity scale: %g m/s per degC", yscale))
detach(air.hal)
}

\author{Dan Kelley}

\keyword{misc}

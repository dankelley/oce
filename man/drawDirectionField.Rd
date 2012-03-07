\name{drawDirectionField}

\alias{drawDirectionField}

\title{Draw a direction field}

\description{Draw a direction field}

\usage{drawDirectionField(x, y, u, v, scalex, scaley, add=FALSE, type=1,
debug=getOption("oceDebug"), \dots)}

\arguments{
    \item{x,y}{coordinates at which velocities are specified}
    \item{u,v}{velocity components in the x and y directions}
    \item{scalex,scaley}{scale to be used for the velocity arrows.  Exactly one
        of these must be specified.  Arrows that have \code{u^2+v^2=1} will
        have length \code{scalex} along the x axis, or \code{scaley} along the
        y axis, according to which parameter is not missing.}
    \item{add}{if \code{TRUE}, the arrows are added to an existing plot;
        otherwise, a new plot is started, with \code{asp} set to 1.}
    \item{type}{type of arrow.  (ignored at present)}
    \item{debug}{debugging value; set to a positive integer to get debugging
        information.}
    \item{\dots}{extra graphical parameters, supplied to functions called by
        \code{drawDirectionField}.}
}

\details{This is a preliminary function, and so the argument list may change.}

\value{None.}

\examples{
library(oce)
plot(c(-1, 1), c(-1, 1))
drawDirectionField(x=c(0, 0), y=c(0, 0), u=c(1,1), v=c(1,-1),scalex=0.5)
}
\author{Dan Kelley}
\keyword{misc}

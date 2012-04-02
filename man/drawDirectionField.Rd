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
        y axis, according to which argument is given.}
    \item{add}{if \code{TRUE}, the arrows are added to an existing plot;
        otherwise, a new plot is started, with \code{asp} set to 1.}
    \item{type}{type of the arrow-like indication of the direction.}
    \item{debug}{debugging value; set to a positive integer to get debugging
        information.}
    \item{\dots}{extra graphical parameters, supplied to functions called by
        \code{drawDirectionField}.  Try adjusting \code{cex}, \code{pch}, and
        \code{lwd}, and \code{col}.}
}

\details{The direction field is indicated variously, depending on the value of
    \code{type}.  
    
    For \code{type=1}, each indicator is drawn with a symbol, according to the
    value of \code{pch} (either supplied globally, or as an element of the
    \code{...} list) and of size \code{cex}, and colour \code{col}.   Then, a
    line segment is drawn for each, and for this \code{lwd} and \code{col} may
    be set globally or in the \code{...} list.

    For \code{type=2}, the points are not drawn, but arrows are drawn instead
    of the line segments.  Again, \code{lwd} and \code{col} control the type of
    the line.
}

\value{None.}

\examples{
library(oce)
plot(c(-1.5, 1.5), c(-1.5, 1.5), xlab="", ylab="", type='n')
drawDirectionField(x=rep(0, 2), y=rep(0, 2), u=c(1,1), v=c(1, -1), scalex=0.5, add=TRUE)
plot(c(-1.5, 1.5), c(-1.5, 1.5), xlab="", ylab="", type='n')
drawDirectionField(x=rep(0, 2), y=rep(0, 2), u=c(1,1), v=c(1, -1), scalex=0.5, add=TRUE, type=2)
plot(c(-1.5, 1.5), c(-1.5, 1.5), xlab="", ylab="", type='n')
drawDirectionField(x=rep(0, 2), y=rep(0, 2), u=c(1,1), v=c(1, -1), scalex=0.5, add=TRUE, type=2, length=0.1)
}
\author{Dan Kelley}
\keyword{misc}

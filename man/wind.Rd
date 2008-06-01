\name{wind}
\docType{wind}
\alias{wind}
\title{Sample (wind) data for contouring}
\description{Wind data, for contouring.}
\usage{
data(wind)
col = hsv(2/3*(wind$u - min(wind$u)) / diff(range(wind$u)), 1, 1)
plot(wind$x, wind$y, col=col, pch=20)
text(wind$x, wind$y, wind$u, pos=4, cex=0.7)
}

\format{Columns \code{wind$x} and \code{wind$y} are location, while
  \code{wind$u} is the wind speed, in m/s.}

\source{The data are taken from Figure 5 of Koch et al. (1983).}

\references{S. E.  Koch and M.  DesJardins and P. J. Kocin, 1983.  ``An
interactive Barnes objective map anlaysis scheme for use with satellite
and conventional data,'' \emph{J.  Climate Appl.  Met.}, vol 22,
p. 1487-1503.}

\author{Dan Kelley}

\keyword{misc}

% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{bilinearInterp}
\alias{bilinearInterp}
\title{Bilinear Interpolation Within a Grid}
\usage{
bilinearInterp(x, y, gx, gy, g)
}
\arguments{
\item{x}{vector of x values at which to interpolate}

\item{y}{vector of y values at which to interpolate}

\item{gx}{vector of x values for the grid}

\item{gy}{vector of y values for the grid}

\item{g}{matrix of the grid values}
}
\value{
vector of interpolated values
}
\description{
This is used by \code{\link{topoInterpolate}}.
}

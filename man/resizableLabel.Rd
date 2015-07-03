% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{resizableLabel}

\alias{resizableLabel}

\title{Provide axis names in adjustable sizes}

\description{Provide axis names in adjustable sizes, e.g. using T instead of
  Temperature, and including units as appropriate.}

\usage{
resizableLabel(item=c("S", "C", "conductivity mS/cm", "conductivity S/m",
  "T", "theta", "sigmaTheta",
  "conservative temperature", "absolute salinity",
  "nitrate", "nitrite", "oxygen", "phosphate", "silicate",
  "tritium", "spice", "fluorescence",
  "p", "z",
  "distance", "distance km", "along-track distance km",
  "heading", "pitch", "roll",
  "u", "v", "w", "speed", "direction",
  "eastward", "northward", "depth", "elevation",
  "latitude", "longitude",
  "frequency cph", "spectral density m2/cph"),
               axis=c("x", "y"), sep)}

\arguments{
  \item{item}{code for the label.}
  \item{axis}{which axis to use.}

  \item{sep}{optional character string inserted between the unit and the
    parentheses or brackets that enclose it. If not provided, then
    \code{\link{getOption}("oceUnitSep")} is checked. If that exists, then it is
    used as the separator; if not, no separator is used.}

}

\value{A character string or expression, in either a long or a shorter format,
  for use in the indicated axis at the present plot size.  Whether the unit is
  enclosed in parentheses or square brackets is determined by the value of
  \code{getOption("oceUnitBracket")}, which may be \code{"["} or \code{"("}.
  Whether spaces are used between the unit and these deliminators is set by
  \code{psep} or \code{\link{getOption}("oceUnitSep")}.}

\details{Used by e.g. \code{\link{plot.ctd}}.}

\author{Dan Kelley}

\keyword{misc}

% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{resizableLabel}

\alias{resizableLabel}

\title{Provide axis names in adjustable sizes}

\description{Provide axis names in adjustable sizes, e.g. using T instead of
  Temperature, and including units as appropriate.}

\usage{
resizableLabel(item, axis, sep)}

\arguments{

  \item{item}{code for the label.  This must be an element from the following
    list, or an abbreviation that uniquely identifies an element through its
    first letters: \code{"S"}, \code{"C"}, \code{"conductivity mS/cm"},
    \code{"conductivity S/m"}, \code{"T"}, \code{"theta"}, \code{"sigmaTheta"},
    \code{"conservative temperature"}, \code{"absolute salinity"},
    \code{"nitrate"}, \code{"nitrite"},
    \code{"oxygen"}, \code{"oxygen saturation"}, \code{"oxygen mL/L"},
    \code{"oxygen umol/L"}, \code{"oxygen umol/kg"},
    \code{"phosphate"},
    \code{"silicate"}, \code{"tritium"}, \code{"spice"}, \code{"fluorescence"},
    \code{"p"}, \code{"z"}, \code{"distance"}, \code{"distance km"},
    \code{"along-track distance km"}, \code{"heading"}, \code{"pitch"},
    \code{"roll"}, \code{"u"}, \code{"v"}, \code{"w"}, \code{"speed"},
    \code{"direction"}, \code{"eastward"}, \code{"northward"}, \code{"depth"},
    \code{"elevation"}, \code{"latitude"}, \code{"longitude"}, \code{"frequency
      cph"}, or \code{"spectral density m2/cph"}.}

  \item{axis}{a string indicating which axis to use; must be \code{x} or
    \code{y}.}

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

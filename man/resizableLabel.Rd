\name{resizableLabel}

\alias{resizableLabel}

\title{Provide axis names in adjustable sizes}

\description{Provide axis names in adjustable sizes, e.g. using T instead of
    Temperature, and including units as appropriate.}

\usage{
resizableLabel(item=c("S", "T", "theta", "sigmaTheta",
  "nitrate", "nitrite", "oxygen", "phosphate", "silicate", "tritium", "spice",
  "p", "z", "distance",
  "heading", "pitch", "roll",
  "u", "v", "w", "speed", "direction",
  "eastward", "northward",
  "elevation"),
  axis=c("x", "y"))}

\arguments{
  \item{item}{code for the label.}
  \item{axis}{which axis to use.}
}

\value{A character string or expression, in either a long or a shorter
  format, for use in the indicated axis at the present plot size.}

\details{Used by e.g. \code{\link{plot.ctd}}.}

\author{Dan Kelley}

\keyword{misc}

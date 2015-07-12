% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{landsatAdd}

\alias{landsatAdd}

\title{Add a band to a landsat image}

\description{Add a band to a landsat image}

\usage{landsatAdd(x, data, name, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{A \code{landsat} object, e.g. as read by \code{\link{read.landsat}}.}

  \item{data}{A matrix of data, with dimensions matching that of entries already in \code{x}.}
  
  \item{name}{Name to be used for the data.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount of debugging
    information, or a higher value for more debugging.}
}

\details{This may be used to add new ``bands'' to a landsat object.  These are
  stored differently than the real bands.  Real bands are stored in one or two
  byte (raw)  matrices (see \code{\link{landsat-class}}) but added bands are
  conventional numerical matrices.  See examples.}
  
\value{An object of \code{\link[base]{class}} \code{"landsat"}, with a new
  ``band''.}

\seealso{The documentation for \code{\link{landsat-class}} explains the
  structure of landsat objects, and also outlines the other functions dealing
  with them.}

\examples{
\dontrun{
library(oce)
## Winter-time Nova Scotia and surrounding seas
ns <- read.landsat("LC80080292014065LGN00", band="tirs1")
tirs1 <- ns[["tirs1"]]
ML <- ns@metadata$header$radiance_mult_band_10
AL <- ns@metadata$header$radiance_add_band_10
K1 <- ns@metadata$header$k1_constant_band_10
K2 <- ns@metadata$header$k2_constant_band_10
Llambda <- ML * tirs1 + AL
d <- K2 / log(K1 / Llambda + 1)
temperature <- d - 273.15
temperature[tirs1 == 0] <- NA
ns <- landsatAdd(ns, temperature, "temperature")
plot(ns, band="temperature")
title(ns[['time']])
}
}

\author{Dan Kelley}

\keyword{misc}


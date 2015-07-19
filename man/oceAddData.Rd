% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{oceAddData}

\alias{oceGetData}
\alias{oceDeleteData}
\alias{oceSetData}
\alias{oceGetMetadata}
\alias{oceDeleteMetadata}
\alias{oceSetMetadata}

\title{Access or alter data or metadata within oce object.}
  

\usage{
oceGetData(object, name, default=NA)
oceDeleteData(object, name)
oceSetData(object, name, value, note="")
oceGetMetadata(object, name, default=NA)
oceDeleteMetadata(object, name)
oceSetMetadata(object, name, value, note="")
}

\arguments{
  \item{object}{An oce object.}

  \item{name}{Name of the desired item.}

  \item{value}{Value for the named item.}

  \item{default}{A default value to be returned if \code{name} is not found.}

  \item{note}{A note to be inserted in the processing log, in addition to an
    automatically-constructed note.}

}

\value{For \code{set} and \code{delete} functions, the updated object is
  returned. For the \code{get} functions, the named item is returned.}

\description{Add, alter, get, or delete data or metadata items.}

\details{In providing access to the \code{data} and \code{metadata} slots of an
  object, these functions are similar to direct access, e.g. in
  \preformatted{
    x <- oceSetMetadata(x, "latitude", 45)
    x@metadata$latitude <- 45
  }
  the first has the advantage of adding useful notes to the
  \code{processingLog}. The \code{delete} functions also update the log.}

\examples{
library(oce)
data("ctd")
oceGetMetadata(ctd, "latitude")
ctd <- oceSetMetadata(ctd, "latitude", 45, "We like round numbers")
summary(ctd)
}


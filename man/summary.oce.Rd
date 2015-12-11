% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{summary.oce}

\alias{summary.oce}
\alias{summary,oce,missing-method}
\alias{summary,oce-method}

\title{Summarize an OCE object}

\description{Summarizes some of the data in a \code{oce} object.}

\usage{\S4method{summary}{oce}(object, \dots)}

\arguments{

  \item{object}{an object of class \code{"oce"}, perhas as read with
    \code{\link{read.netcdf}}.}

  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented, including the station name,
  sampling location, data ranges, etc.}

\value{An object of class \code{"summary.oce"}, which when printed shows
  pertinent information about the OCE object and its processing.}

\seealso{The documentation for \code{\link{oce-class}} explains the structure of
  OCE objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}

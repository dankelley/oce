\name{is.beam}

\alias{is.beam}
\alias{is.xyz}
\alias{is.enu}
\alias{coordinate}

\title{Determine coordinate system for acoustic-doppler device}

\description{Determine coordinate system for acoustic-doppler device}

\usage{
is.beam(x)
is.xyz(x)
is.enu(x)
coordinate(x)
}

\details{These functions work by checking the value of
  \code{x$metadata$oce.coordinate}, the main purpose being to prevent
  users from having to know the difference between that item and others
  of similar names.}

\arguments{
  \item{x}{an \code{oce} object that inherits from either \code{adp}
    or \code{adv}.}
}

\value{The \code{is.} functions return \code{TRUE} if the object
  inherits from \code{adv} or \code{adp}, and is of the stated
  coordinate type, or \code{FALSE} otherwise.  \code{coordinate} return
  a string indicating the coordinate system, \code{beam} (velocities
  oriented along acoustic beams), \code{xyz} (velocities in a cartesian
  coordinate system tied to the instrument, or its pressure case), or
  \code{enu} (a cartesian coordinate system with one component pointing
  east, the second north, and the third up.}

\seealso{\code{\link{read.adp}} and \code{\link{read.adv}} read such
  files; consult their documentation for information about other related
  functions.}

\examples{
data(adp)
print(is.beam(adp))
print(is.xyz(adp))
print(is.enu(adp))
print(coordinate(adp))
}

\author{Dan Kelley}

\keyword{misc}

% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{fullFilename}
\alias{fullFilename}
\title{full name of file, including path}
\description{Return full name of file, including path}

\usage{fullFilename(filename)}

\arguments{
  \item{filename}{name of file}
}

\details{Determines the full name of a file, including the path.  Used by many
  \code{read.X} routines, where \code{X} is the name of a class of object.  This
  is a wrapper around \code{\link{normalizePath}}, with warnings turned off so
  that messages are not printed for unfound files (e.g. URLs).}

\value{Full file name}

\author{Dan Kelley}

\keyword{misc}

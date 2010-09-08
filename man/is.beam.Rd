\name{is.beam}

\alias{is.beam}
\alias{is.xyz}
\alias{is.enu}

\title{Determine coordinate system for acoustic-doppler device}

\description{Determine coordinate system for acoustic-doppler device}

\usage{
is.beam(x)
is.xyz(x)
is.enu(x)
}

\details{These functions work by checking the value of
  \code{x$metadata$oce.coordinate}, the main purpose being to prevent
  users from having to know the difference between that item and others
  of similar names.}

\arguments{
  \item{x}{an \code{oce} object that inherits from either \code{adp}
    or \code{adv}.}
}

\value{\code{TRUE} if the object inherits from \code{adv} or \code{adp},
  and is of the stated coordinate type, or \code{FALSE} otherwise.}

\seealso{\code{\link{read.adp}} and \code{\link{read.adv}} read such
  files; consult their documentation for information about other related
  functions.}

\examples{
data(adp)
print(is.beam(adp))
print(is.xyz(adp))
print(is.enu(adp))
}

\author{Dan Kelley}

\keyword{misc}

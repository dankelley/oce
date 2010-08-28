\name{vector.show}
\alias{vector.show}
\title{Show some values from a vector}
\description{Show some values from a vector}
\usage{vector.show(v, msg)}
\arguments{
\item{v}{the vector.}
\item{msg}{a message to show, introducing the vector.  If not provided,
  then a label is created from \code{v}.}
}

\details{This is similar to \code{\link{str}}, but it shows data at the
  first and last of the vector, which is quite helpful in debugging.}

\value{A string, suitable for using in \code{\link{cat}} or \code{\link{oce.debug}}.}

\author{Dan Kelley}
\keyword{misc}

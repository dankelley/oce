\name{summary.aquadopp}

\alias{summary.aquadopp}
\alias{print.summary.aquadopp}

\title{Summarize an aquadopp object}

\description{Summarize data in an \code{aquadopp} object.}

\usage{
\method{summary}{aquadopp}(object, \dots)

\method{print}{summary.aquadopp}(x, digits=max(6, getOption("digits")-1), \dots)
}

\arguments{
  \item{object}{an object of class \code{"aquadopp"}, usually, a result of a
    call to \code{\link{read.aquadopp}}, \code{\link{read.oce}}.}
  \item{x}{an object of class \code{"summary.aquadopp"}, usually, a result of a
    call to \code{summary.aquadopp}.}
  \item{digits}{the number of significant digits to use when printing.}
  \item{\dots}{further arguments passed to or from other methods.}
}

\details{Pertinent summary information is presented.}

\value{An object of class \code{"summary.aquadopp"}, which contains
  pertinent information about the AQUADOPP record and its processing.}

\seealso{A \code{aquadopp} object may  be read with \code{\link{read.aquadopp}} or
  \code{\link{read.oce}}.}

\references{General information on the aquadopp profiler, and other
  NorTek products, is available at \url{http://www.nortekusa.com/}.}

\examples{
\dontrun{
library(oce)
data(aquadopp)
summary(aquadopp)
}
}

\author{Dan Kelley}

\keyword{misc}

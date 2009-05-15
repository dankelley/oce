\name{resizable.label}

\alias{resizable.label}

\title{Provide axis names in adjustable sizes}

\description{Provide axis names in adjustable sizes, e.g. using T
  instead of Temperature.}

\usage{resizable.label(item=c("S", "T", "p", "z", "distance"), axis=c("x", "y"))}

\arguments{
  \item{item}{code for the label.}
  \item{axis}{which axis to use.}
}

\value{A character string or expression, in either a long or a shorter
  format, for use in the indicated axis at the present plot size.}

\details{Used by e.g. \code{\link{plot.ctd}}.}

\author{Dan Kelley}

\keyword{misc}

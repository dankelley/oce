\name{magic}

\alias{magic}

\title{Find the type of an oceanographic data file}

\description{Find the type of an oceanographic data file, by examining the first
line.}

\usage{magic(file, debug=getOption("oce.debug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to be checked.}
  \item{debug}{an integer, set non-zero to turn on debugging.  Higher
  values indicate more debugging.}
}

\details{This function tries to infer the file type by inspecing some portion of the start of the file.}

\value{A character string indicating the file type, or \code{"unknown"}, if the type cannot be determined.}

\seealso{This is used mainly by \code{\link{read.oce}}.}

\author{Dan Kelley}

\keyword{misc}

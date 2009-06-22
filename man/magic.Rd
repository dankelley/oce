\name{magic}

\alias{magic}

\title{Find the type of an oceanographic data file}

\description{Find the type of an oceanographic data file, by examining the first
line.}

\usage{magic(file, debug=FALSE)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to be checked.}
  \item{debug}{a boolean, set to \code{TRUE} to print intermediate steps
    in the investigation of file type.}
}

\details{This function tries to infer the file type by inspecing some portion of the start of the file.}

\value{A character string indicating the file type, or \code{"unknown"}, if the type cannot be determined.}

\seealso{This is used mainly by \code{\link{read.oce}}.}

\author{Dan Kelley}

\keyword{misc}

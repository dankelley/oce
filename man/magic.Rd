\name{magic}
\alias{magic}
\title{Find the type of an oceanographic data file}
\description{
Find the type of an oceanographic data file, by examining the first
line.
}
\usage{
magic(file)
}
\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to be checked.}
}
\details{
  This function tries to infer the file type from the first line.
}
\value{
  A character string indicating the file type, or \code{"unknown"}, if the type cannot be determined.
}

\seealso{
This is used mainly by \code{\link{read.oce}}.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}

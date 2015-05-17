\name{oceMagic}

\alias{oceMagic}
\alias{oce.magic}

\title{Find the type of an oceanographic data file}

\description{Find the type of an oceanographic data file.}

\usage{oceMagic(file, debug=getOption("oceDebug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to be checked.}
  \item{debug}{an integer, set non-zero to turn on debugging.  Higher
  values indicate more debugging.}
}

\details{This function tries to infer the file type, based on either the data within the file or, more rarely, based on the file name.}

\value{A character string indicating the file type, or \code{"unknown"}, if the type cannot be determined. If the
result contains \code{"/"} characters, these separate a list describing the file type, with the first element
being the general type, the second element being the manufacturer, and the third element being the manufacturer's
name for the instrument. For example, \code{"adp/nortek/aquadopp"} indicates a acoustic-doppler profiler made by
NorTek, of the model type called AquaDopp.}

\seealso{This is used mainly by \code{\link{read.oce}}.}

\author{Dan Kelley}

\keyword{misc}

\name{read.oce}

\alias{read.oce}

\title{Read an oceanographic data file}

\description{Read an oceanographic data file, auto-discovering the file type from the
first line of the file.}

\usage{read.oce(file, ...)}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{...}{arguments to be handed to whichever instrument-specific reading function 
is selected, based on the header.}
}

\details{
  This function tries to infer the file type from the first line, using
  \code{\link{oceMagic}}.  If it can be discovered, then an instrument-specific
  file reading function is called, with the \code{file} and with any additional
  arguments being supplied.
}

\value{An object of base \code{\link[base]{class}} \code{"oce"}, and also with a class
that signifies the type of data, e.g. \code{"ctd"}, \code{"sealevel"}, etc.}

\seealso{
The file type is determined by 
\code{\link{oceMagic}}.  If the file type can be determined, then one 
of the following is called:
\code{\link{read.ctd}},
\code{\link{read.coastline}}
\code{\link{read.lobo}}, 
\code{\link{read.logger}}, 
\code{\link{read.sealevel}}, etc.
}

\examples{
library(oce)
x <- read.oce(system.file("extdata", "ctd.cnv", package="oce"))
plot(x) # summary with TS and profiles
plotTS(x) # just the TS
}

\author{Dan Kelley}

\keyword{misc}

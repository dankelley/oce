\name{teosSetLibrary}

\alias{teosSetLibrary}

\title{Set location of TEOS-10 library}

\description{Set location of TEOS-10 library}

\usage{teosSetLibrary(path)}

\arguments{
  \item{path}{full path to the TEOS-10 shared library.}
}

\details{This need not be called if the library is in
    the file \code{/usr/local/lib/libgswteos-10.so}, which is set by default
    when Oce starts up.}
        
\value{None.}

\seealso{See \code{\link{teos}} for more information on the TEOS-10 library.}

\author{Dan Kelley}

\keyword{misc}

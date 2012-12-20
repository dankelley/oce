\name{mapLocator}

\alias{mapLocator}

\title{Locate points on a existing map}

\description{Locate points on an existing map}

\usage{mapLocator(n=512, type='n')}

\arguments{

    \item{n}{number of points to locate; see \code{\link{locator}}.}

    \item{type}{type of connector for the points; see \code{\link{locator}}.}
}

\details{This uses \code{\link{map2lonlat}} to infer the location in
    geographical space; see the documentation for that function on its
    limitations.}
    
\seealso{See \code{\link{mapPlot}} for general information on plotting maps,
    including other functions.}

\author{Dan Kelley}

\keyword{hplot}

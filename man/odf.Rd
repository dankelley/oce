\name{odf}

\docType{data}

\alias{odf}

\title{A CTD profile on the Scotian Shelf}

\description{This was created from the ODF file provided with Oce, using
    \preformatted{
    odf <- read.odf(system.file("extdata", "CTD_BCD2014666_008_1_DN.ODF", package="oce")) 
    }}

\usage{data(odf)}

\examples{
\dontrun{
library(oce)
data(odf)
plot(odf, span=500, fill='lightgray') # span/fill show location better
## several levels have bad QC flags
subset(odf, flag!=0)
}
}

\seealso{The documentation for \code{\link{odf-class}} explains the structure
    of \code{odf} objects, and also outlines the other functions dealing with
    them.}

\keyword{datasets}


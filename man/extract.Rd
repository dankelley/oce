\name{extract}
\alias{extract}
\title{Extract data from an oce object}
\description{Extract data from an oce object}
\usage{extract(x, names)}
\arguments{
\item{x}{an \code{oce} object.}
\item{names}{a vector of character values, indicating names of items to be extracted.}
}

\details{This is a convenience function that extracts data from an \code{oce}
    object, providing the user with isolation from the internal storage scheme
    used in \code{oce} objects.}

\value{A list of the values found.}

\examples{
library(oce)
## ctd cast
data(ctd)
TS <- extract(ctd, c("temperature", "salinity"))
print(summary(lm(temperature~salinity, data=TS)))

## section
data(section)
TSlon <- extract(section, c("temperature","salinity","pressure","longitude"))
med <- factor(-20 > TSlon$longitude, labels=c("med","other"))
plotTS(TSlon, col=c("black","gray")[med])
}

\author{Dan Kelley}

\keyword{misc}


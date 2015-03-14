\name{as.lisst}

\alias{as.lisst}

\title{Coerce data into a lisst object}

\description{Coerce data into a lisst object}

\usage{as.lisst(data, filename="", year=0, tz="UTC", longitude=NA, latitude=NA)}

\arguments{
  \item{data}{A table (or matrix) containing 42 columns, as in a LISST data file.}
  \item{filename}{Name of file containing the data.}
  \item{year}{Year in which the first observation was made.  This is necessary
      because LISST timestamps do not indicate the year of observation.  The
      default value is odd enough to remind users to include this argument.}
  \item{tz}{Timezone of observations.  This is necessary because LISST
      timestamps do not indicate the timezone.}
  \item{longitude}{Longitude of observation.}
  \item{latitude}{Latitude of observation.}
}

\details{If \code{data} contains fewer than 42 columns, an error is reported.
    If it contains more than 42 columns, only the first 42 are used.  This is
    used by \code{\link{read.lisst}}, the documentation on which explains the
    meanings of the columns.}

\value{An object of \code{\link[base]{class}} \code{"lisst"} (for details,
  see \code{\link{read.lisst}}).}

\seealso{The documentation for \code{\link{lisst-class}} explains the structure
    of LISSTobjects, and also outlines the other functions dealing with them.}


\examples{
library(oce)
}

\references{The LIST-100 users guide (version 4.65), which provided the
    information for this function, was downloaded in late May 2012, from
    \code{http://www.sequoiasci.com/products/fam_LISST_100.aspx}, a link that
    no longer worked in March 2015.}

\author{Dan Kelley}

\keyword{misc}

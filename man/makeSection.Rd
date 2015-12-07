\name{makeSection}
\alias{makeSection}

\title{Bind CTD profiles together into a cross section [DEPRECATED]}

\description{Combine a series of CTD profiles together to create a section.
    This function is deprecated, meaning that \strong{it will be removed two
        CRAN releases from now}. Please use \code{\link{as.section}} instead.}

\usage{makeSection(item, \dots)}

\arguments{
  \item{item}{either (1) a \code{ctd} object, in which case the rest of the
    arguments are other \code{ctd} objects, (2) a list of \code{ctd}
    objects, (3) a list of names of \code{ctd} objects, or (4) a list of
    names of files containing \code{ctd} data.}
  \item{\dots}{one or more \code{ctd} objects, either given as separate
    arguments, or a list of such objects.}
}

\details{The stations are stored in order of the station identification number
    (stored as \code{metadata$station} in the ctd object), if possible.  The
    \code{ctd} stations must share identical pressure values; use
    \code{\link{sectionGrid}} to do that.}

\value{An object of \code{\link[base]{class}} \code{section}.}

\seealso{See \code{\link{section-class}} for information on section objects and
    functions that deal with them.}

\examples{
library(oce)
data(ctd)
ctd2 <- ctd
ctd2[["temperature"]] <- ctd[["temperature"]] + 0.5
ctd2[["salinity"]] <- ctd[["salinity"]] + 0.1
ctd2[["longitude"]] <- ctd[["longitude"]] + 0.01
section <- makeSection(ctd, ctd2)
summary(section)
# Below is how to create a section from well-named CSV files
# that contain consecutive stations
\dontrun{
    plot(sectionSmooth(sectionGrid(makeSection(dir("*.csv")))))
}
}

\author{Dan Kelley}

\keyword{misc}

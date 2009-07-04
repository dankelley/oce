\name{ctd.add.column}
\alias{ctd.add.column}

\title{Add a column to a CTD file}

\description{Add a column to a \code{ctd} file, updating the header as appropriate.}

\usage{
ctd.add.column(x, column, name="", label, unit, debug=FALSE)
}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{column}{A column of data to be inserted.  The length of this
    column must match the length of the columns already in the objecct.}
  \item{name}{String indicating the name this column will have in the
    dataframe; e.g. if set to \code{hello} in creating a \code{"ctd"}
    called \code{d}, then the column would be accessed later as
    \code{d$data$hello}.}
  \item{label}{Optional string indicating the name of the column, as
    it will appear in plot labels.  (If not given, \code{name} will be
    used.}
  \item{unit}{Optional string indicating the unit for the column, 
    as it will appear in plot labels.}
  \item{debug}{Set \code{TRUE} to see information about the processing.}
}

\details{These functions add the column to the the data frame in the
  the object.

  Note that the arguments to this function were changed in version
  0.1.77 of the package, in order to remove confusion about their
  function, and in order to make the function better able to deal with
  a wider range of CTD devices.
}

\value{An object of \code{\link[base]{class}} \code{"ctd"}, with a new column.}

\seealso{See \code{\link{read.ctd}} for the data format.}

\examples{
library(oce)
data(ctd) 
SS <- ctd$data$salinity^2
ctd.new <- ctd.add.column(ctd, SS, "ss", "S^2", "PSU^2")
}

\author{Dan Kelley}
\keyword{misc}

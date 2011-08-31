\name{ctdAddColumn}
\alias{ctdAddColumn}

\title{Add a column to a CTD file}

\description{Add a column to a \code{ctd} file, updating the header as appropriate.}

\usage{
ctdAddColumn(x, column, name, label, debug=FALSE)
}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{column}{A column of data to be inserted, in the form of a
    numeric vector, whose length matches that of columns in the
    objecct.}
  \item{name}{Character string indicating the name this column is to
    have in the \code{data} dataframe.  For example, using
    \code{name="hello"} in the creation of \code{d} would mean
    that the data could be accessed as \code{d$data$hello} or
    \code{d$data[["hello"]]}.}
  \item{label}{Optional character string or expression indicating the
    name of the column, as it will appear in plot labels.  (If not
    given, \code{name} will be used.)}
  \item{debug}{Set \code{TRUE} to see information about the processing.}
}

\details{
  These functions add the column to the the data frame in the the object.
}

\note{
  Note that the arguments to this function were changed in version
  0.1.77 of the package, in order to remove confusion about their
  function, and in order to make the function better able to deal with a
  wider range of CTD devices.
}

\value{
  An object of \code{\link[base]{class}} \code{"ctd"}, with a new column.
}

\seealso{
  See \code{\link{read.ctd}} for the data format.
}

\examples{
library(oce)
data(ctd) 
SS <- ctd$data$salinity^2
ctdNew <- ctdAddColumn(ctd, SS, "ss", 
                        expression(paste(S^2, " [", PSU^2, " ]")))
plotProfile(ctdNew, "ss")
}

\author{Dan Kelley}
\keyword{misc}

\name{ctdAddColumn}
\alias{ctdAddColumn}

\title{Add a column to a CTD file}

\description{Add a column to a \code{ctd} file, updating the header as appropriate.}

\usage{ctdAddColumn(x, column, name, label, unit=NULL, debug=getOption("oceDebug"))}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{column}{A column of data to be inserted, in the form of a
    numeric vector, whose length matches that of columns in the
    objecct.}
  \item{name}{Character string indicating the name this column is to
    have in the \code{data} slot of \code{x}.}
  \item{label}{Optional character string or expression indicating the
    name of the column, as it will appear in plot labels.  (If not
    given, \code{name} will be used.)}
  \item{unit}{Optional indication of the unit, in the form of a list containing
    items \code{unit}, which is an expression, and \code{scale}, which is a
      character string. For example, modern measurements of temperature have
      unit \code{list(name=expression(degree*C), scale="ITS-90")}.}
  \item{debug}{set to a positive value to get debugging information during processing.}
}

\details{
  This function adds a column to the object's \code{data} slot.
}


\value{An object of \code{\link[base]{class}} \code{"ctd"}, with a new column.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
library(oce)
data(ctd) 
SS <- ctd[["salinity"]]^2
ctdNew <- ctdAddColumn(ctd, SS, "ss", 
expression(paste(S^2, " [", PSU^2, " ]")))
plotProfile(ctdNew, "ss")
}

\author{Dan Kelley}
\keyword{misc}

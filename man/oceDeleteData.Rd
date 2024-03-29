% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{oceDeleteData}
\alias{oceDeleteData}
\title{Delete Something From the data Slot of an oce Object}
\usage{
oceDeleteData(object, name)
}
\arguments{
\item{object}{an \linkS4class{oce} object.}

\item{name}{String indicating the name of the item to be deleted.}
}
\description{
Return a copy of the supplied object that lacks the named
element in its \code{data} slot, and that has a note
about the deletion in its processing log.
}
\seealso{
Other things related to the data slot: 
\code{\link{oceGetData}()},
\code{\link{oceRenameData}()},
\code{\link{oceSetData}()}
}
\author{
Dan Kelley
}
\concept{things related to the data slot}

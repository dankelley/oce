% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/accessors.R
\name{oceSetData}
\alias{oceSetData}
\title{Set Something in the data Slot of an oce Object}
\usage{
oceSetData(object, name, value, unit, originalName = "-", note = "")
}
\arguments{
\item{object}{an \linkS4class{oce} object.}

\item{name}{the name of the \code{data} item to be set.}

\item{value}{value for the item.}

\item{unit}{an optional indication of the units for the item. This has
three possible forms (see \dQuote{Details}).}

\item{originalName}{character string giving an 'original' name (e.g.
as stored in the header of a data file). The default, \code{"-"}, is
taken by \code{summary()} functions to mean that there was no original
name in \code{object}.}

\item{note}{either empty (the default), a character string, or \code{NULL},
to control additions made to the processing log of the return value. If
\code{note=""} then the an entry is created based on deparsing the function call.
If \code{note} is a non-empty string, then that string gets added added
to the processing log. Finally, if \code{note=NULL}, then nothing is
added to the processing log. This last form is useful in cases where
\code{oceSetData} is to be called many times in succession, resulting
in an overly verbose processing log; in such cases, it might help
to add a note by e.g. \code{processingLog(a) <- "QC (memo dek-2018-01/31)"}}
}
\value{
\code{oceSetData} returns an \linkS4class{oce} object, the \code{data} slot of which
has been altered either by adding a new item or modifying an existing item.
}
\description{
Create a copy of an object in which some element of its
\code{data} slot has been altered, or added.
}
\details{
The trickiest argument to set is the \code{unit}.  There are three
possibilities for this:
\enumerate{
\item \code{unit} is a named or unnamed \code{\link[=list]{list()}} that contains two items.
If the list is named, the names must be
\code{unit} and \code{scale}. If the list is unnamed, the stated names are assigned
to the items, in the stated order. Either way, the \code{unit}
item must be an \code{\link[=expression]{expression()}} that specifies the unit,
and the \code{scale} item must be a string that describes the scale. For
example, modern temperatures have
\code{unit=list(unit=expression(degree*C), scale="ITS-90")}.
\item \code{unit} is an \code{\link[=expression]{expression()}} giving the unit as above. In this
case, the scale will be set to \code{""}.
\item \code{unit} is a character string that is converted
into an expression with \link{parse}\code{(text=unit)},
and the scale set to \code{""}.
}
}
\examples{
data(ctd)
Tf <- swTFreeze(ctd)
ctd <- oceSetData(ctd, "freezing", Tf,
    unit = list(unit = expression(degree * C), scale = "ITS-90")
)
plotProfile(ctd, "freezing")

}
\seealso{
Other things related to the data slot: 
\code{\link{oceDeleteData}()},
\code{\link{oceGetData}()},
\code{\link{oceRenameData}()}
}
\author{
Dan Kelley
}
\concept{things related to the data slot}

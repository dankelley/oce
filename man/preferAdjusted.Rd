% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/argo.R
\name{preferAdjusted}
\alias{preferAdjusted}
\title{Set Preference for Adjusted Values}
\usage{
preferAdjusted(argo, which = "all", fallback = TRUE)
}
\arguments{
\item{argo}{An \linkS4class{argo} object.}

\item{which}{A character vector naming the items for which
(depending also on the value of \code{fallback}) adjusted values
are to be sought by future calls to \code{\link{[[,argo-method}}.
The short names are used, e.g. \code{which="oxygen"} means that
adjusted oxygen is to be returned in future calls
such as \code{argo[["oxygen"]]}.  The default,
\code{"all"}, means to  use adjusted values for any item in \code{argo}
that has adjusted values.}

\item{fallback}{A logical value indicating whether to fall back
to unadjusted values for any data field in which the
adjusted values are all \code{NA}.  The default value, \code{TRUE},
avoids a problem with biogeochemical fields, where adjustment
of any one field may lead to insertion of "adjusted" values for
other fields that consist of nothing more than \code{NA}s.}
}
\value{
An \linkS4class{argo} object its \code{metadata} slot altered
(in its \code{adjustedWhich} and \code{adjustedFallback} elements)
as a signal for how \code{\link{[[,argo-method}} should
function on the object.
}
\description{
\linkS4class{argo} data can contain "adjusted" forms of data items,
which may be more trustworthy than the original
data, and \code{preferAdjusted} lets the user express a preference
for such adjusted data.  This means that using
\code{\link{[[,argo-method}} on the results returned by \code{preferAdjusted}
will (if possible) return adjusted data, and also use those adjusted
data in computations of derived quantities such as Absolute Salinity.
The preference applies also to units and to data-quality flags,
both of which can be returned by \code{\link{[[,argo-method}}, as
discussed in \dQuote{Details}.
}
\details{
\code{preferAdjusted()} merely sets two items in the \code{metadata} slot of the
returned \linkS4class{argo} object. The real action is carried out by
\code{\link{[[,argo-method}} but, for convenience, the details are explained here.

Consider salinity, for example.
If \code{which} equals \code{"all"}, or if it is a character
vector containing \code{"salinity"}, then using
\code{\link{[[,argo-method}} on the returned object
will yield the adjusted forms of the salinity data,
its associated flags, or its units.  Thus, in the salinity
case,
\itemize{
\item \code{argo[["salinity"]]} will attempt to return \code{argo@data$salinityAdjusted}
instead of returning \code{argo@data$salinity}, although if the adjusted values
are all \code{NA} then, depending on the value of \code{fallback}, the
unadjusted values may be returned; similarly
\item \code{argo[["salinityFlags"]]} will attempt to return
\code{argo@metadata$flags$salinityAdjusted}
instead of \code{argo@metadata$flags$salinity}, and
\item \code{argo[["salinityUnits"]]} will attempt to return
\code{argo@metadata$units$salinityAdjusted}
instead of \code{argo@metadata$units$salinity}.
}

The default value, \code{which="all"}, indicates that this
preference for adjusted values will apply to all the
elements of the \code{data} slot of the returned object, along
with associated flags and units. This can be handy for quick
work, but analysts may also choose to restrict their use of
adjusted values to a subset of variables, based on their own
decisions about data quality or accuracy.

The default value \code{fallback=TRUE} indicates that later calls to
\code{\link{[[,argo-method}} should return unadjusted values for any
data items that have \code{NA} for all the adjusted values.  This
condition is rare for core variables (salinity, temperature and
pressure) but is annoyingly common for biogeochemical variables; see
e.g. Section 2.2.5 of Reference 1 for a discussion of
the conditions under which Argo NetCDF files contain
adjusted values. Setting \code{fallback=FALSE} means that adjusted
values (if they exist) will always be returned, even if they
are a useless collection of \code{NA} values.

Error fields, such as \code{salinityAdjustedError}, are returned
as-is by \code{\link{[[,argo-method}}, regardless of whether
the object was created by \code{preferAdjusted}.

It should be noted that, regardless of whether \code{preferAdjusted}
has been used, the analyst can always access either unadjusted
or adjusted data directly, using the original variable names stored
in the source NetCDF file.  For example, \code{argo[["PSAL"]]}
yields unadjusted salinity values, and
\code{argo[["PSAL_ADJUSTED"]]} yields adjusted values (if they exist, or
\code{NULL} if they do not).
Similarly, adjusted value can always be obtained by using a form
like \code{argo[["salinityAdjusted"]]}.
}
\examples{
library(oce)
data(argo)
argoAdjusted <- preferAdjusted(argo)
all.equal(argo[["salinityAdjusted"]], argoAdjusted[["salinity"]])
all.equal(argo[["salinityFlagsAdjusted"]], argoAdjusted[["salinityFlags"]])
all.equal(argo[["salinityUnitsAdjusted"]], argoAdjusted[["salinityUnits"]])

}
\references{
\enumerate{
\item Argo Data Management Team. "Argo User's Manual V3.3." Ifremer,
November 28, 2019.
\doi{10.13155/29825}
}
}
\author{
Dan Kelley, based on discussions with Jaimie Harbin (with
respect to the \code{\link{[[,argo-method}} interface) and Clark Richards
(with respect to storing the preference in the \code{metadata} slot).
}

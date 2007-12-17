\name{read.ctd}
\alias{read.ctd}
\title{Read a CTD data file}
\description{
Read a CTD data file, producing an object of type \code{ctd}.
}
\usage{
read.ctd(file, type=NULL, debug=FALSE,
 	columns=NULL, station="", check.human.headers=FALSE)
}
\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{type}{if \code{NULL}, then the first line is studied, in order to determine the
    file type.  If \code{type="SBE19"}, then a \emph{Seabird 19} (or similar) CTD
    format is assumed. If \code{type="WOCE"} then a 
    WOCE-exchange file is assumed.}
  \item{debug}{a flag that can be set to \code{TRUE} to turn on debugging.}
  \item{columns}{if \code{NULL}, then \code{read.ctd} tries to infer column names from the
    header.  If a list, then it will be taken to be the list
    of columns.  The list must include \code{"pressure"}, \code{"temperature"}
    and either \code{"conductivity"} or \code{"salinity"}, or else very little
    can be done with the file.}
  \item{station}{optional character string containing an identifying name (or
    number) for the station. (This can be useful if the routine cannot determine
    the name automatically, or if another name is preferred.)}
  \item{check.human.headers}{produces warnings for missing human-written header items.}
}
\details{
  Oceanographers use CTD (conductivity-temperature-depth) instruments
  to measure key properties of the ocean, such as water temperature,
  salinity, etc.  This function reads CTD datasets that have been stored
  in common formats, and could be extended to 
  accommodate other formats if needed.

  \code{read.ctd} does a reasonable job of inferring meta-information
  from file headers, but it still has limitations.  For
  example, in the first file tested during development, 
  the sampling rate was written as
  \code{* sample rate = 1 scan every 0.5 seconds}, while 
  in the second test file it was written 
  \code{* Real-Time Sample Interval = 0.125 seconds}.  Similarly,
  \code{read.ctd} can be challenged in parsing latitudes and longitudes
  in the wide variety of ways that humans choose.  Still, such limitations
  are not really pressing in practice, since the \code{ctd} object is made available
  for manipulation.  If \code{read.ctd} cannot scan \code{33 and a third} as a latitude,
  just examine the header (stored as a list in \code{object$header}), and do something
  like \code{object$latitude <- 33 + 1/3}.

  It should be noted that different file types provide different meta-information.
  For example, the WOCE exchange format binds together the institute name and the
  initials of the chief scientist into a single string that \code{read.ctd} cannot
  parse, so both \code{object$institute} and \code{object$scientist} are left 
  blank for WOCE files.
}
\value{
  A \code{ctd} object, which is a list with elements detailed below.  The
  most important elements are the station name and position, along
  with the profile data that are contained in the data frame named \code{data}.
  (Other elements in the list may be deleted in future versions of the
  package, if they prove to be of little use in practice, or if they prove
  to have been idiosyncratic features of the particular files used in
  early development of \code{oce}.)
  \item{header}{the header itself, normally containing several dozen lines of
    information.}
  \item{filename}{name of the file passed to \code{read.ctd}.}
  \item{filename.orig}{name of the original file saved by the instrument
    (normally a hex file).}
  \item{system.upload.time}{system upload time.}
  \item{ship}{name of the ship from which the CTD was deployed.}
  \item{scientist}{name of the scientist taking the data.}
  \item{institute}{name of the institute.}
  \item{address}{the address of the institute where the scientist.}
  \item{cruise}{name of cruise.}
  \item{station}{station number or name.}
  \item{date}{date of lowering of CTD into the water.}
  \item{start.time}{time when instrument started recording data.}
  \item{latitude}{latitude, in decimal degrees, positive north of equator.}
  \item{longitude}{longitude, in decimal degrees, positive if east
    of Greenwich and west of dateline.}
  \item{recovery}{date of recovery of CTD.}
  \item{water.depth}{the water depth at the site.}
  \item{sample.interval}{time interval between samples, in seconds.}
  \item{processing.log}{a processing log.}
  \item{data}{a data table containing the profile data.  The column
    names are discovered from the header, and may differ from file
    to file.  For example, some CTD instruments may have a fluorometer
    connected, others may not.  The order of the columns may vary from
    case to case, and so it is important to refer to them by name.
    The following vectors are normally present: \code{data$pressure},
    \code{data$salinity}, \code{data$temperature}, and
    \code{data$sigma.theta}. (The last of these is calculated using
    \code{\link{sw.sigma.theta}}, whether or not values are present
    in the data file.)}
}

\seealso{
The generic function \code{\link{read.oce}} provides an alternative to this.

Data from a CTD cast may be assembled into a \code{ctd} object using \code{\link{as.ctd}}.  
A \code{ctd} object may be summarized with \code{\link{summary.ctd}}.
Overview plots may be made with \code{\link{plot.ctd}}, while
\code{\link{plot.TS}} produces TS plots and
\code{\link{plot.ctd.scan}} produces scan plots that may help with data editing.
Extraneous data such as those collected during upcasts and equilibration intervals
may be trimmed with \code{\link{ctd.trim}},
and the data may be cast onto specified pressure levels with \code{\link{ctd.decimate}}.
Low-level manipulation may be done with \code{\link{ctd.add.column}}
and \code{\link{ctd.update.header}}, as well as by direct manipulation of the items
within \code{ctd} objects.
}
\examples{
\dontrun{
library(oce)
x <- read.ctd("/usr/local/lib/R/library/oce/demo/ctdprofile.cnv")
plot(x) # summary with TS and profiles
plot.TS(x) # just the TS
}
}
\references{
The Sea-Bird SBE 19plus profiler is described at
\url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
The company recommends the use of their own software, and perhaps for this reason
it is difficult to find a specification for the data files.  Inspection of
data files led to most of the code used in Oce.  If the company ever publishes
standards for the data formats, of course Oce will be adjusted.  In the meantime,
it does a reasonable job in many instances.

The WOCE-exchange format is described at
\url{http://www.nodc.noaa.gov/woce_V2/disk02/exchange/exchange_format_desc.htm}
}
\author{Dan Kelley}
\keyword{misc}

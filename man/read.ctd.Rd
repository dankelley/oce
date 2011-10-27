\name{read.ctd}

\alias{read.ctd}
\alias{read.ctd.sbe}
\alias{read.ctd.woce}
\alias{read.ctd.odf}

\title{Read a CTD data file}

\description{Read a CTD data file, producing an object of type \code{ctd}.}

\usage{
read.ctd(file, type=NULL, columns=NULL, station=NULL, 
  monitor=FALSE, debug=getOption("oceDebug"), processingLog, \dots)
read.ctd.sbe(file, columns=NULL, station=NULL, missing.value,
  monitor=FALSE, debug=getOption("oceDebug"), processingLog, \dots)
read.ctd.woce(file, columns=NULL, station=NULL, missing.value=-999, 
  monitor=FALSE, debug=getOption("oceDebug"), processingLog, \dots)
read.ctd.odf(file, columns=NULL, station=NULL, missing.value=-999, 
  monitor=FALSE, debug=getOption("oceDebug"), processingLog, \dots)
}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{type}{if \code{NULL}, then the first line is studied, in order to determine the
    file type.  If \code{type="SBE19"}, then a \emph{Seabird 19} (or similar) CTD
    format is assumed. If \code{type="WOCE"} then a 
    WOCE-exchange file is assumed.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
    moderate amount of debugging information, or to 2 to get more.} 
  \item{columns}{if \code{NULL}, then \code{read.ctd} tries to infer column names from the
    header.  If a list, then it will be taken to be the list
    of columns.  The list must include \code{"pressure"}, \code{"temperature"}
    and either \code{"conductivity"} or \code{"salinity"}, or else very little
    can be done with the file.}
  \item{station}{optional character string containing an identifying name (or
    number) for the station. (This can be useful if the routine cannot determine
    the name automatically, or if another name is preferred.)}
  \item{missing.value}{optional missing-value flag; data matching this value will
    be set to \code{NA} upon reading.}
  \item{monitor}{boolean, set to \code{TRUE} to provide an indication
    of line of data read.  (This may be useful in debugging problems in
    reading files that are misconfigured.)}
  \item{processingLog}{if provided, the action item to be stored in the
    log.  (Typically only provided for internal calls; the default that
    it provides is better for normal calls by a user.)}
  \item{\dots}{additional arguments, passed to called routines.}
}

\details{These functions read CTD datasets that have been stored in
  common formats, and could be extended to accommodate other formats if
  needed.  \code{read.ctd} is a dispatch function that analyzes some of
  the file contents, and then calls either \code{read.ctd.sbe} (for
  files created by Seabird CTD instruments) or \code{read.ctd.woce} (for
  files stored in the exchange format used by the World Ocean
  Circulation Experiment.)

  The functions attempt to infer a wide range of meta-information from
  file headers, but variations in these headers impose some limitations
  on the process.  For example, in the first file tested during
  development, the sampling rate was written as \code{* sample rate = 1
  scan every 0.5 seconds}, while the second test file had \code{*
  Real-Time Sample Interval = 0.125 seconds}; dealing with such
  variations is not easy.  Similarly, the functions can be challenged in
  parsing latitudes and longitudes in the wide variety of ways that
  humans choose (decimal versus degree-minute-second, signed versus
  "east" and "west", or "e" and "w".).  The solution to the challenge is
  simple: do a reasonable job with common file formats, and provide
  users with the tools to correct (or insert) information that would
  ideally be gleaned from the headers.  This can be done by direct
  manipulation of the entries in the object's \code{metadata},
  e.g. writing \code{object@metadata$latitude <- 33 + 1/3}.

  It should be noted that different file types provide different
  meta-information.  For example, the WOCE exchange format binds
  together the institute name and the initials of the chief scientist
  into a single string that \code{read.ctd} cannot parse, so both
  \code{object@metadata$institute} and \code{object@metadata$scientist} are
  left blank for WOCE files.}

\value{An object of \code{\link[base]{class}} \code{"ctd"}, which is a
  list with elements detailed below. 
  The most important elements are the station name and position, along
  with the profile data that are contained in the data frame named \code{data}.
  (Other elements in the list may be deleted in future versions of the
  package, if they prove to be of little use in practice, or if they prove
  to have been idiosyncratic features of the particular files used in
  early development of \code{oce}.)
  \item{data}{a data table containing the profile data.  The column
    names are discovered from the header, and may differ from file
    to file.  For example, some CTD instruments may have a fluorometer
    connected, others may not.  The order of the columns may vary from
    case to case, and so it is important to refer to them by name.
    The following vectors are normally present: \code{data$pressure},
    \code{data$salinity}, \code{data$temperature}, and
    \code{data$sigmatheta}. (\eqn{\sigma_\theta}{sigma-theta} is calculated
    using \code{\link{swSigmaTheta}}.)}
  \item{metadata}{a list containing the following items
    \describe{
      \item{\code{header}}{the header itself, normally containing
	several dozen lines of information.}
      \item{\code{filename}}{name of the file passed to \code{read.ctd}.}
      \item{\code{filename.orig}}{name of the original file saved by the
	instrument (normally a hex file).}
      \item{\code{system.upload.time}}{system upload time.}
      \item{\code{ship}}{name of vessel from which the CTD was deployed.}
      \item{\code{scientist}}{name of the scientist leading the work at sea.}
      \item{\code{institute}}{name of the institute behind the work.}
      \item{\code{address}}{the address of the institute where the
	scientist works.}
      \item{\code{cruise}}{name of cruise.}
      \item{\code{station}}{station number or name.}
      \item{\code{date}}{date of lowering of CTD into the water.}
      \item{\code{startTime}}{time when instrument started recording data.}
      \item{\code{latitude}}{latitude, in decimal degrees, positive
	north of equator.}
      \item{\code{longitude}}{longitude, in decimal degrees, positive if
	east of Greenwich and west of dateline.}
      \item{\code{recovery}}{date of recovery of CTD.}
      \item{\code{waterDepth}}{the water depth at the site.}
      \item{\code{sampleInterval}}{time interval between samples, in
	seconds.}
    }
  }
  \item{processingLog}{a processingLog of processing, in the standard \code{oce} format.}
}

\seealso{The documentation for \code{\link{sealevel-class}} explains the
    structure of sealevel objects, and also outlines the other functions
    dealing with them.}

\section{note}{In many cases, CTD instruments are set up to report dates in
    English.  This can cause a problem for users running in different locales,
    since e.g month names differ.  Therefore, if you know your datafile is
    written in American-English notation, you might want to do
    \code{Sys.setlocale("LC_TIME", "en_US")} before you try to read the data.}

\examples{
library(oce)
\dontrun{
woce <- read.oce(
  "http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/example_ct1.csv")
plot(woce)
}
}

\references{The Sea-Bird SBE 19plus profiler is described at
    \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.  The
    company recommends the use of their own software, and perhaps for this
    reason it is difficult to find a specification for the data files.
    Inspection of data files led to most of the code used in Oce.  If the
    company ever publishes standards for the data formats, of course Oce will
    be adjusted.  In the meantime, it does a reasonable job in many instances.

    The WOCE-exchange format is described at
    \url{http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm},
    and a sample file is at
    \url{http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/example_ct1.csv}

    The ODF format, used by the Canadian Department of Fisheries and Oceans, is
    described at
    \url{http://slgo.ca/app-sgdo/en/docs_reference/documents.html}, and this
    was used as a base for \code{read.ctd.odf}.  However, it was only a
    starting point, for examination of data files revealed many variants in the
    names of the data columns.  If anything odd happens with ODF files (e.g. if
    they cannot be plotted), the first thing to do is to reread the files with
    \code{debug=1}, to see if column names were converted properly.

}


\author{Dan Kelley}

\keyword{misc}

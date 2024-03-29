% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.aml.R
\name{read.ctd.aml}
\alias{read.ctd.aml}
\title{Read a ctd File in AML Format}
\usage{
read.ctd.aml(
  file,
  format,
  encoding = "UTF-8-BOM",
  debug = getOption("oceDebug"),
  processingLog,
  ...
)
}
\arguments{
\item{file}{a connection or a character string giving the name of
the file to load.}

\item{format}{an integer indicating the format type.  If not supplied, the
first line is examined to determine whether the file matches the \code{format=1} or
\code{format=2} style (see \dQuote{Details}).}

\item{encoding}{a character value that indicates the encoding to be used for
this data file, if it is textual.  The default value for most functions is
\code{"latin1"}, which seems to be suitable for files containing text written in
English and French.}

\item{debug}{an integer specifying whether debugging information is
to be printed during the processing. This is a general parameter that
is used by many \code{oce} functions. Generally, setting \code{debug=0}
turns off the printing, while higher values suggest that more information
be printed. If one function calls another, it usually reduces the value of
\code{debug} first, so that a user can often obtain deeper debugging
by specifying higher \code{debug} values.}

\item{processingLog}{ignored.}

\item{...}{ignored.}
}
\value{
\code{\link[=read.ctd.aml]{read.ctd.aml()}} returns a \linkS4class{ctd} object.
}
\description{
\code{\link[=read.ctd.aml]{read.ctd.aml()}} reads files that hold data acquired with an AML
Oceanographic BaseX2 CTD instrument. The SeaCast software associated with
this device can output data in several formats, of which only two are
handled, and only one is recommended (see \dQuote{Details}).
}
\details{
The handled formats match files available to the author, both of which
diverge slightly from the format described in the AML documentation (see
\dQuote{References}).

Regardless of the format, files must contain columns named \code{Conductivity (mS/cm)}, \code{Temperature (C)} and \code{Pressure (dBar)}, because \linkS4class{ctd}
objects need those quantities.  (Actually, if pressure is not found, but
\code{Depth (m)} is, then pressure is estimated with \code{\link[=swDepth]{swDepth()}}, as a
workaround.) Note that other columns will be also read and stored in the
returned value, but they will not have proper units.  Attempts are made to
infer the sampling location from the file, by searching for strings like
\verb{Latitude=} in the header. Headers typically contain two values of the
location, and it is the second pair that is used by this function, with a
\code{NA} value being recorded if the value in the file is \code{no-lock}.  The
instrument serial number is also read, although the individual serial numbers
of the sensors are not read.  Position and serial number are stored in the
the \code{metadata} slot of the returned value.  The entire header is also stored
there, to let users glean more about dataset.

Two formats are handled, as described below. Format 1 is greatly preferred,
because it is more robust (see below on \code{format=2}) and also because it can
be read later by the AML SeaCast software.
\enumerate{
\item If \code{format} is \code{1} then the file is assumed to be in a format created by
selecting \emph{Export As ... Seacast (.csv)} in AML's SeaCast software, with
settings to output pressure (or, as second-best, depth), temperature and
conductivity, and perhaps other things.  The delimiter must be comma.  If
date and time are output, their formats must be yyyy-mm-dd and UTC,
respectively.  Decoding the file proceeds as follows.  First, a check is done
to ensure that the first line consists of the string \verb{[cast header]}. Then an
attempt is made to infer location and serial number from the header.  After
this, \code{\link[=read.ctd.aml]{read.ctd.aml()}} searches down for a line containing the string
\verb{[data]}. The first line thereafter is taken as a comma-separated list of
variable names, and lines following that are taken to hold the variable
values, separated by commas.
\item If \code{format} is \code{2} then the first line must be a comma-separated list of
column names.  This may be followed by header information, which is handled
similarly as for \code{format=1}. The data are read from all lines that have the
same number of commas as the first line, an admittedly brittle strategy
developed as a way to handle some files that lacked other information about
the end of the header.
}

In both cases, the data columns, renamed to oce convention, are stored in the
\code{data} slot.  For the mandatory variables, units are also stored, as for
other \linkS4class{ctd} objects.
}
\examples{
library(oce)
f <- system.file("extdata", "ctd_aml.csv.gz", package = "oce")
d <- read.ctd.aml(f)
summary(d)

}
\references{
AML Oceanographic. "SeaCast 4 User Manual (Version 2.06)." AML Oceanographic,
Mahy 2016.
\verb{https://www.subseatechnologies.com/media/files/page/032e50ac/seacast-4-2-user-manual-sti.pdf}.
}
\seealso{
Other things related to ctd data: 
\code{\link{CTD_BCD2014666_008_1_DN.ODF.gz}},
\code{\link{[[,ctd-method}},
\code{\link{[[<-,ctd-method}},
\code{\link{as.ctd}()},
\code{\link{cnvName2oceName}()},
\code{\link{ctd}},
\code{\link{ctd-class}},
\code{\link{ctd.cnv.gz}},
\code{\link{ctdDecimate}()},
\code{\link{ctdFindProfiles}()},
\code{\link{ctdFindProfilesRBR}()},
\code{\link{ctdRaw}},
\code{\link{ctdRepair}()},
\code{\link{ctdTrim}()},
\code{\link{ctd_aml.csv.gz}},
\code{\link{d200321-001.ctd.gz}},
\code{\link{d201211_0011.cnv.gz}},
\code{\link{handleFlags,ctd-method}},
\code{\link{initialize,ctd-method}},
\code{\link{initializeFlagScheme,ctd-method}},
\code{\link{oceNames2whpNames}()},
\code{\link{oceUnits2whpUnits}()},
\code{\link{plot,ctd-method}},
\code{\link{plotProfile}()},
\code{\link{plotScan}()},
\code{\link{plotTS}()},
\code{\link{read.ctd}()},
\code{\link{read.ctd.itp}()},
\code{\link{read.ctd.odf}()},
\code{\link{read.ctd.odv}()},
\code{\link{read.ctd.saiv}()},
\code{\link{read.ctd.sbe}()},
\code{\link{read.ctd.ssda}()},
\code{\link{read.ctd.woce}()},
\code{\link{read.ctd.woce.other}()},
\code{\link{setFlags,ctd-method}},
\code{\link{subset,ctd-method}},
\code{\link{summary,ctd-method}},
\code{\link{woceNames2oceNames}()},
\code{\link{woceUnit2oceUnit}()},
\code{\link{write.ctd}()}

Other functions that read ctd data: 
\code{\link{read.ctd}()},
\code{\link{read.ctd.itp}()},
\code{\link{read.ctd.odf}()},
\code{\link{read.ctd.saiv}()},
\code{\link{read.ctd.sbe}()},
\code{\link{read.ctd.ssda}()},
\code{\link{read.ctd.woce}()},
\code{\link{read.ctd.woce.other}()}
}
\author{
Dan Kelley
}
\concept{functions that read ctd data}
\concept{things related to ctd data}

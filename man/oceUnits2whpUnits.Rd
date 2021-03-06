% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.woce.R
\name{oceUnits2whpUnits}
\alias{oceUnits2whpUnits}
\title{Translate oce unit to WHP unit}
\usage{
oceUnits2whpUnits(units, scales)
}
\arguments{
\item{units}{vector of expressions for units in oce notation.}

\item{scales}{vector of strings for scales in oce notation.}
}
\value{
vector of strings holding WOCE-style names.
}
\description{
Translate oce units to WHP-style strings,
to match patterns.
}
\references{
Several online sources list WOCE names. An example is
\verb{https://geo.h2o.ucsd.edu/documentation/manuals/pdf/90_1/chap4.pdf}
(a link that was noticed to be failing on May 31, 2020).
}
\seealso{
Other things related to ctd data: 
\code{\link{CTD_BCD2014666_008_1_DN.ODF.gz}},
\code{\link{[[,ctd-method}},
\code{\link{[[<-,ctd-method}},
\code{\link{as.ctd}()},
\code{\link{cnvName2oceName}()},
\code{\link{ctd-class}},
\code{\link{ctd.cnv}},
\code{\link{ctdDecimate}()},
\code{\link{ctdFindProfiles}()},
\code{\link{ctdRaw}},
\code{\link{ctdTrim}()},
\code{\link{ctd}},
\code{\link{d200321-001.ctd}},
\code{\link{d201211_0011.cnv}},
\code{\link{handleFlags,ctd-method}},
\code{\link{initialize,ctd-method}},
\code{\link{initializeFlagScheme,ctd-method}},
\code{\link{oceNames2whpNames}()},
\code{\link{plot,ctd-method}},
\code{\link{plotProfile}()},
\code{\link{plotScan}()},
\code{\link{plotTS}()},
\code{\link{read.ctd.itp}()},
\code{\link{read.ctd.odf}()},
\code{\link{read.ctd.odv}()},
\code{\link{read.ctd.sbe}()},
\code{\link{read.ctd.woce.other}()},
\code{\link{read.ctd.woce}()},
\code{\link{read.ctd}()},
\code{\link{setFlags,ctd-method}},
\code{\link{subset,ctd-method}},
\code{\link{summary,ctd-method}},
\code{\link{woceNames2oceNames}()},
\code{\link{woceUnit2oceUnit}()},
\code{\link{write.ctd}()}

Other functions that interpret variable names and units from headers: 
\code{\link{ODFNames2oceNames}()},
\code{\link{cnvName2oceName}()},
\code{\link{oceNames2whpNames}()},
\code{\link{unitFromStringRsk}()},
\code{\link{unitFromString}()},
\code{\link{woceNames2oceNames}()},
\code{\link{woceUnit2oceUnit}()}
}
\author{
Dan Kelley
}
\concept{functions that interpret variable names and units from headers}
\concept{things related to ctd data}

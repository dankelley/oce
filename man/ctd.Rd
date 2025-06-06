% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ctd.R
\docType{data}
\name{ctd}
\alias{ctd}
\title{Sample ctd Data}
\usage{
data(ctd)
}
\description{
This is a CTD profile measured in Halifax Harbour in 2003, based
on \code{\link[=ctdRaw]{ctdRaw()}}, but trimmed to just the downcast with
\code{\link[=ctdTrim]{ctdTrim()}}, using indices inferred by inspection of the
results from \code{\link[=plotScan]{plotScan()}}.
}
\details{
This station was sampled by students enrolled in the Dan Kelley's Physical
Oceanography class at Dalhousie University.  The data were acquired near the
centre of the Bedford Basin of the Halifax Harbour, during an October 2003
field trip of Dalhousie University's Oceanography 4120/5120 class.  Note
that the \code{startTime} in the \code{metadata} slot was altered from 1903 to 2003,
using \code{\link[=oceEdit]{oceEdit()}}. The change was done because the original time was clearly
incorrect, perhaps owing to the use of software that was designed to work in
the twentieth century only.
}
\section{Sample of Usage}{

\preformatted{
library(oce)
data(ctd)
plot(ctd)
}
}

\seealso{
The full profile (not trimmed to the downcast) is available as
\code{data(ctdRaw)}.

Other datasets provided with oce: 
\code{\link{adp}},
\code{\link{adv}},
\code{\link{amsr}},
\code{\link{argo}},
\code{\link{cm}},
\code{\link{coastlineWorld}},
\code{\link{ctdRaw}},
\code{\link{echosounder}},
\code{\link{landsat}},
\code{\link{lisst}},
\code{\link{lobo}},
\code{\link{met}},
\code{\link{ocecolors}},
\code{\link{rsk}},
\code{\link{sealevel}},
\code{\link{sealevelTuktoyaktuk}},
\code{\link{section}},
\code{\link{topoWorld}},
\code{\link{wind}},
\code{\link{xbt}}

Other things related to ctd data: 
\code{\link{CTD_BCD2014666_008_1_DN.ODF.gz}},
\code{\link{[[,ctd-method}},
\code{\link{[[<-,ctd-method}},
\code{\link{argo2ctd}()},
\code{\link{as.ctd}()},
\code{\link{cnvName2oceName}()},
\code{\link{ctd-class}},
\code{\link{ctd.cnv.gz}},
\code{\link{ctdDecimate}()},
\code{\link{ctdFindProfiles}()},
\code{\link{ctdFindProfilesRBR}()},
\code{\link{ctdRaw}},
\code{\link{ctdRepair}()},
\code{\link{ctdTrim}()},
\code{\link{ctd_aml_type1.csv.gz}},
\code{\link{ctd_aml_type3.csv.gz}},
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
\code{\link{read.ctd.aml}()},
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
}
\concept{datasets provided with oce}
\concept{things related to ctd data}

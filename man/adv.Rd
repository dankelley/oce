% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adv.R
\docType{data}
\name{adv}
\alias{adv}
\title{Sample adv Data}
\source{
This file came from the SLEIWEX-2008 experiment.
}
\usage{
data(adv)
}
\description{
This \linkS4class{adv} object is a sampling of measurements made with a
Nortek Vector acoustic Doppler velocimeter deployed as part of the St Lawrence
Internal Wave Experiment (SLEIWEX).  Various identifying features have been
redacted.
}
\examples{
\donttest{
library(oce)
data(adv)

# Velocity time-series
plot(adv)

# Spectrum of upward component of velocity, with ``turbulent'' reference line
s <- spectrum(adv[["v"]][, 3], plot = FALSE)
plot(log10(s$freq), log10(s$spec), type = "l")
for (a in seq(-20, 20, by = 1)) {
    abline(a = a, b = -5 / 3, col = "gray", lty = "dotted")
}
}

}
\seealso{
Other datasets provided with oce: 
\code{\link{adp}},
\code{\link{amsr}},
\code{\link{argo}},
\code{\link{cm}},
\code{\link{coastlineWorld}},
\code{\link{ctd}},
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

Other things related to adv data: 
\code{\link{[[,adv-method}},
\code{\link{[[<-,adv-method}},
\code{\link{adv-class}},
\code{\link{advSontekAdrFileTrim}()},
\code{\link{applyMagneticDeclination,adv-method}},
\code{\link{beamName}()},
\code{\link{beamToXyz}()},
\code{\link{enuToOther}()},
\code{\link{enuToOtherAdv}()},
\code{\link{plot,adv-method}},
\code{\link{read.adv}()},
\code{\link{read.adv.nortek}()},
\code{\link{read.adv.sontek.adr}()},
\code{\link{read.adv.sontek.serial}()},
\code{\link{read.adv.sontek.text}()},
\code{\link{rotateAboutZ}()},
\code{\link{subset,adv-method}},
\code{\link{summary,adv-method}},
\code{\link{toEnu}()},
\code{\link{toEnuAdv}()},
\code{\link{velocityStatistics}()},
\code{\link{xyzToEnu}()},
\code{\link{xyzToEnuAdv}()}
}
\concept{datasets provided with oce}
\concept{things related to adv data}

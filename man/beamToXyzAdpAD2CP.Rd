% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/adp.nortek.ad2cp.R
\name{beamToXyzAdpAD2CP}
\alias{beamToXyzAdpAD2CP}
\title{Convert From Beam to XYZ Coordinates (AD2CP adp Data)}
\usage{
beamToXyzAdpAD2CP(x, debug = getOption("oceDebug"))
}
\arguments{
\item{x}{an \linkS4class{adp} object.}

\item{debug}{an integer specifying whether debugging information is
to be printed during the processing. This is a general parameter that
is used by many \code{oce} functions. Generally, setting \code{debug=0}
turns off the printing, while higher values suggest that more information
be printed. If one function calls another, it usually reduces the value of
\code{debug} first, so that a user can often obtain deeper debugging
by specifying higher \code{debug} values.}
}
\description{
This looks at all the items in the \code{data} slot of \code{x}, to
see if they contain an array named \code{v} that holds velocity.
If that velocity has 4 components, and if \code{oceCoordinate} for
the item is \code{"beam"}, then
along-beam velocity components \eqn{B_1}{B1}
\eqn{B_2}{B1}, \eqn{B_3}{B3}, and \eqn{B_4}{B4}
are converted to instrument-oriented Cartesian velocity components \eqn{u}{u}
\eqn{v}{v} and \eqn{w}{w}
using the convex-geometry formulae from section 5.5 of reference 1,
viz.
\eqn{u=ca(B_1-B_2)}{u=a*(B1-B2)}, \eqn{v=ca(B_4-B_3)}{v=a*(B4-B3)},
\eqn{w=-b(B_1+B_2+B_3+B_4)}{w=-b*(B1+B2+B3+B4)}. In addition to these,
an estimate of the
error in velocity is computed as
\eqn{e=d(B_1+B_2-B_3-B_4)}{e=d*(B1+B2-B3-B4)}.
The geometrical factors in these formulae are:
\eqn{a=1/(2\sin\theta)}{a=1/(2*sin(theta))}
where \eqn{\theta}{theta} is the angle the beams make to the axial direction
(which is available as \code{x[["beamAngle"]]}),
\eqn{b=1/(4\cos\theta)}{b=1/(4*cos(theta))}, and
\eqn{d=a/\sqrt{2}}{d=a/sqrt(2)}.
}
\references{
\enumerate{
\item Teledyne RD Instruments.
\dQuote{ADCP Coordinate Transformation: Formulas and Calculations,}
January 2010. P/N 951-6079-00.
}
}
\seealso{
Other things related to adp data: 
\code{\link{[[,adp-method}},
\code{\link{[[<-,adp-method}},
\code{\link{ad2cpCodeToName}()},
\code{\link{ad2cpHeaderValue}()},
\code{\link{adp}},
\code{\link{adp-class}},
\code{\link{adpAd2cpFileTrim}()},
\code{\link{adpConvertRawToNumeric}()},
\code{\link{adpEnsembleAverage}()},
\code{\link{adpFlagPastBoundary}()},
\code{\link{adpRdiFileTrim}()},
\code{\link{adp_rdi.000}},
\code{\link{applyMagneticDeclination,adp-method}},
\code{\link{as.adp}()},
\code{\link{beamName}()},
\code{\link{beamToXyz}()},
\code{\link{beamToXyzAdp}()},
\code{\link{beamToXyzAdv}()},
\code{\link{beamUnspreadAdp}()},
\code{\link{binmapAdp}()},
\code{\link{enuToOther}()},
\code{\link{enuToOtherAdp}()},
\code{\link{handleFlags,adp-method}},
\code{\link{is.ad2cp}()},
\code{\link{plot,adp-method}},
\code{\link{read.adp}()},
\code{\link{read.adp.ad2cp}()},
\code{\link{read.adp.nortek}()},
\code{\link{read.adp.rdi}()},
\code{\link{read.adp.sontek}()},
\code{\link{read.adp.sontek.serial}()},
\code{\link{read.aquadopp}()},
\code{\link{read.aquadoppHR}()},
\code{\link{read.aquadoppProfiler}()},
\code{\link{rotateAboutZ}()},
\code{\link{setFlags,adp-method}},
\code{\link{subset,adp-method}},
\code{\link{subtractBottomVelocity}()},
\code{\link{summary,adp-method}},
\code{\link{toEnu}()},
\code{\link{toEnuAdp}()},
\code{\link{velocityStatistics}()},
\code{\link{xyzToEnu}()},
\code{\link{xyzToEnuAdp}()},
\code{\link{xyzToEnuAdpAD2CP}()}
}
\concept{things related to adp data}

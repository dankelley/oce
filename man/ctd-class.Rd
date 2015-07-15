% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{ctd-class}

\docType{class}

\alias{ctd-class}
\alias{[[,ctd-method}
\alias{[[<-,ctd-method}
\alias{show,ctd-method}

\title{Class to store hydrographic data}

\description{Class to store hydrographic data such as measured with a CTD
  (conductivity, temperature, depth) instrument.}

\section{Methods}{

  Consider an object named \code{ctd}.

  \emph{Accessing metadata.}

  Metadata (contained in the S4 slot named \code{metadata}) may be retrieved or
  set by name, \code{ctd[["longitude"]] <- ctd[["longitude"]] + 1} corrects a
  one-degree error.

  \emph{Accessing measured data.}

  Column data may be accessed by name, e.g. \code{ctd[["salinity"]]},
  \code{ctd[["temperature"]]}, \code{ctd[["pressure"]]}, etc.  
    
  Temperature is stored in the ITS-90 scale within the object, but the IPTS-68
  value can be accessed with e.g.  \code{ctd[["temperature68"]]}, and this must
  be done in using various seawater functions, e.g. the density function
  \code{\link{swRho}}, if the UNESCO formulation is being requested.
    
  The TEOS-10 notation for these quantities also works, with \code{ctd[["SP"]]},
  \code{ctd[["t"]]} and \code{ctd[["p"]]} returning identical values to those
  returned for the longer names.  

  After the names listed above have been checked, the remaining names in the
  \code{data} slot are checked using \code{\link{pmatch}}, so that e.g.
  \code{ctd[["sal"]]} will recover practical \code{salinity}, \code{ctd[["sc"]]}
  will recover \code{scan} (if it exists), etc.

  \emph{Accessing derived data.}

  Depth is accessed with e.g. \code{ctd[["depth"]]}, while its negative, the
  vertical coordinate, is accessed with e.g.  \code{ctd[["z"]]}; note that these
  are calculated using \code{\link{swDepth}} and \code{\link{swZ}}, and that any
  values that may have been read in a data file are ignored. 
    
  Potential temperature defined according to UNESCO-1980 is calculated with
  \code{ctd[["theta"]]} or \code{ctd[["potential temperature"]]}.  Salinity is
  retrieved with \code{ctd[["S"]]} or \code{ctd[["salinity"]]}.

  Conservative Temperature defined according to TEOS-2010 is calculated with
  \code{ctd[["CT"]]} or \code{ctd[["conservative temperature"]]}.  Absolute
  salinity is calculated with \code{ctd[["SA"]]} or \code{ctd[["absolute
    salinity"]]}. Note that the salinity calculation requires a latitude and
  longitude, and if the \code{ctd} object lacks those data, the values 300E and
  30N will be used as a default.
    
  The square of buoyancy frequency is retrieved with \code{ctd[["N2"]]} or
  \code{\link{swN2}}, density ratio with \code{ctd[["Rrho"]]} and spiciness with
  \code{ctd[["spice"]]}.

  \emph{Assigning values.}

  Items stored in the object may be altered with e.g.  \code{ctd[["salinity"]]
    <- rep(35,10)}.  For obvious reasons, this does not work with derived
  quantities such as conservative temperature, etc.

  \emph{Overview of contents.}

  The \code{show} method (e.g.  \code{show(ctd)}) displays information about
  the object.

}

\seealso{A file containing CTD profile data may be read with
  \code{\link{read.ctd}}, and a CTD object can also be created with
  \code{\link{as.ctd}}.  See \code{\link{read.ctd}} for references on data
  formats used in CTD files.
  
  Statistical summaries are provided by \code{\link{summary.ctd}}, while
  \code{\link{show}} displays an overview.
  
  CTD objects may be plotted with \code{\link{plot.ctd}}, which does much of its
  work by calling \code{\link{plotProfile}} or \code{\link{plotTS}}, both of
  which can also be called by the user, to get fine control over the plots.

  A CTD profile can be isolated from a larger record with \code{\link{ctdTrim}},
  a task made easier when \code{\link{plotScan}} is used to examine the results.
  Towyow data can be split up into sets of profiles (ascending or descending)
  with \code{\link{ctdFindProfiles}}.  CTD data may be smoothed and/or cast onto
  specified pressure levels with \code{\link{ctdDecimate}}.  
  
  Low-level manipulation may be done with functions such as
  \code{\link{ctdAddColumn}} and \code{\link{ctdUpdateHeader}}.  Additionally,
  many of the contents of CTD objects may be altered with the \code{[[]]} scheme
  discussed above, and skilled uses may also manipulate the contents directly.

}

\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


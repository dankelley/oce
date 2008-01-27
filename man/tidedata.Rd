\name{tidedata}

\docType{data}

\alias{tidedata}

\title{Tidal constituent information}

\description{Tidal information, for use in fitting tidal
  models to sealevel data, in the form of a 
  \code{\link[base]{list}} named \code{tidedata}, which
  in turn contains three lists:
  \code{tidedata} is a \code{\link[base]{list}} containing 
  \item{\code{const}}{which is a list containing vectors named
    \code{name} (a string with constituent name),
    \code{freq} (frequency in cycles per hour),
    \code{kmpr} (a string naming the comparison constituent),
    \code{ikmpr} (index of comparison constituent),
    \code{df} (frequency difference betwee constituent and its comparison,
    used in the Rayleigh criterion),
    \code{d1} (first Doodson number),
    \code{d2},
    \code{d3}, 
    \code{d4},
    \code{d5},
    \code{d6} (sixth Doodson number),
    \code{semi},
    \code{nsat} (number of satellite constituents),
    \code{ishallow},
    \code{nshallow},
    \code{doodsonamp},
    and \code{doodsonspecies}.}
  \item{\code{sat}}{which is a list containing vectors
    \code{deldood},
    \code{phcorr},
    \code{amprat},
    \code{ilatfac},
    and \code{iconst}.}
  \item{\code{shallow}}{which is a list containing vectors
    \code{iconst},
    \code{coef},
    and \code{iname}.
  }
  Apart from the use of \code{d1} through \code{d6}, the naming and
  content follows \code{T_TIDE}, and the test suite checks
  against some values against their \code{T_TIDE} counterparts.
}

\usage{data(tidedata)}

\source{The data come from the \code{tide3.dat} file of the
  \code{T_TIDE} package (Pawlowicz et al., 2002), and derive from
  Appendices provided by Foreman (1977).  The data are scanned using
  \file{tests/tide.R} in this package, which also performs some tests using
  \code{T_TIDE} values as a reference.
}

\references{
  Foreman, M. G. G., 1977.
  Manual for tidal heights analysis and prediction.
  Pacific Marine Science Report 77-10,
  Institute of Ocean Sciences, Patricia Bay, Sidney, BC, 58pp.
  
  Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.
  Classical tidal harmonic analysis including error estimates in MATLAB using \code{T\_TIDE}.
  Computers and Geosciences, 28, 929-937.
}

\author{Dan Kelley}

\keyword{misc}

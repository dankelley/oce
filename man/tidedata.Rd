\name{tidedata}

\docType{data}

\alias{tidedata}

\title{Tidal constituent information}

\description{Tidal information, for use in fitting tidal
  models to sealevel data, in the form of a 
  \code{\link[base]{list}} named \code{"tidedata"}, which
  in turn contains three lists:
  \code{tidedata} is a \code{\link[base]{list}} containing 
  \item{"const"}{which is a list containing vectors named
    \code{"name"} (a string with constituent name),
    \code{"freq"} (frequency in cycles per hour),
    \code{"kmpr"} (a string naming the comparison constituent),
    \code{"ikmpr"} (index of comparison constituent),
    \code{"df"} (frequency difference betwee constituent and its comparison,
    used in the Rayleigh criterion),
    \code{"d1"} (first Doodson number),
    \code{"d2"},
    \code{"d3"}, 
    \code{"d4"},
    \code{"d5"},
    \code{"d6"} (sixth Doodson number),
    \code{"semi"},
    \code{"nsat"},
    \code{"ishallow"},
    \code{"nshallow"},
    \code{"doodsonamp"},
    and \code{"doodsonspecies"}.}
  \item{tidedata$sat}{which is a list containing vectors
    \code{"deldood"},
    \code{"phcorr"},
    \code{"amprat"},
    \code{"ilatfac"},
    and \code{"iconst"}.}
  and
  \item{tidedata$shallow}{which is a list containing
    \code{"iconst"},
    \code{"coef"},
    and \code{"iname"}.
  }
  Apart from the use of \code{"d1"} through \code{"d6"}, the naming and
  content follows \code{T_TIDE} precisely, and the test suite checks
  against values in that package.

}

\usage{data(tidedata)}

\format{See section 1.3 of Foreman (1977).}

\source{The data come from the \code{tide3.dat} file of the
  \code{T_TIDE} package (Pawlowicz et al., 2002), and derive from
  Appendix 7.1 of Foreman (1977).  The data are scanned using
  \file{tests/tide.R} in this package, which also tests the veracity of
  the data, to a limited extent.
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

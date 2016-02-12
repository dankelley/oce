\name{tidedata}

\docType{data}

\alias{tidedata}

\title{Tidal constituent information}

\description{The \code{tidedata} dataset contains Tide-constituent information
    that is use by \code{\link{tidem}} to fit tidal models.
  \code{tidedata} is a list containing
  \describe{
    \item{const}{a list containing vectors
      \describe{
	\item{\code{name}}{a string with constituent name}
	\item{\code{freq}}{the frequency, in cycles per hour}
	\item{\code{kmpr}}{a string naming the comparison constituent, blank if there is none}
	\item{\code{ikmpr}}{index of comparison constituent, or \code{0} if there is none}
	\item{\code{df}}{frequency difference betwee constituent and its comparison, used in the Rayleigh criterion}
	\item{\code{d1}}{first Doodson number}
	\item{\code{d2}}{second Doodson number}
	\item{\code{d3}}{third Doodson number}
	\item{\code{d4}}{fourth Doodson number}
	\item{\code{d5}}{fifth Doodson number}
	\item{\code{d6}}{sixth Doodson number}
	\item{\code{semi}}{(fill in some info later)}
	\item{\code{nsat}}{number of satellite constituents}
	\item{\code{ishallow}}{(fill in some info later)}
	\item{\code{nshallow}}{(fill in some info later)}
	\item{\code{doodsonamp}}{(fill in some info later)}
	\item{\code{doodsonspecies}}{(fill in some info later)}
      }
    }
    \item{\code{sat}}{a list containing vectors
      \describe{
	\item{\code{deldood}}{(fill in some info later)}
	\item{\code{phcorr}}{(fill in some info later)}
	\item{\code{amprat}}{(fill in some info later)}
	\item{\code{ilatfac}}{(fill in some info later)}
	\item{\code{iconst}}{(fill in some info later)}
      }
    }
    \item{\code{shallow}}{a list containing vectors
      \describe{
	\item{\code{iconst}}{(fill in some info later)}
	\item{\code{coef}}{(fill in some info later)}
	\item{\code{iname}}{(fill in some info later)}
      }
    }
  }
  Apart from the use of \code{d1} through \code{d6}, the naming and
  content follows \code{T_TIDE}.  All of this is based on Foreman
  (1977), to which the reader is referred for details.
}

\usage{data(tidedata)}

\source{The data come from the \code{tide3.dat} file of the
  \code{T_TIDE} package (Pawlowicz et al., 2002), and derive from
  Appendices provided by Foreman (1977).  The data are scanned using
  \file{tests/tide.R} in this package, which also performs some tests using
  \code{T_TIDE} values as a reference.
}

\seealso{\code{\link{tidem}} uses this dataset to establish its harmonic
    constituents.}
    
\references{Foreman, M. G. G., 1977.  Manual for tidal heights analysis and
    prediction.  Pacific Marine Science Report 77-10, Institute of Ocean
    Sciences, Patricia Bay, Sidney, BC, 58pp.
  
  Pawlowicz, Rich, Bob Beardsley, and Steve Lentz, 2002.  Classical tidal
  harmonic analysis including error estimates in MATLAB using \code{T_TIDE}.
  Computers and Geosciences, 28, 929-937.  }

\author{Dan Kelley}

\keyword{datasets}

\concept{tide}

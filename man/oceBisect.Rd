\name{oceBisect}

\alias{oceBisect}

\title{Find the root of a function by bisection}

\description{Find the root of a function by bisection}

\usage{oceBisect(f, xleft, xright, dx, debug=getOption("oceDebug"))}

\arguments{

    \item{f}{a function of a single variable, returning a single variable.}

    \item{xleft}{a value of the independent variable, to the left of a root.}

    \item{xright}{a value of the independent variable, to the right of a root.}

    \item{dx}{optional value indicating the desired tolerance of the independent
        variable, near the root.  If not provided, this is taken as
        \code{(xlight-xleft)/1e5}.}

    \item{debug}{a debugging parameter, set nonzero to make \code{oceBisect}
        print a trail of its choices of independent variable, as it searches for
        the root.}

}

\details{The root is found by a classical bisection search.  That is, the first
    step involves defining \code{xmiddle} as the mid-point between \code{xleft}
    and \code{xright}.  Then \code{oceBisect} checks whether the sign of the
    function at \code{xmiddle} matches that at \code{xleft}.  If the sign of the
    function at \code{xmiddle} is the reverse of the sign at \code{xleft}, then a
    root must be between these two points, and so \code{xmiddle} is taken as the
    new \code{xright}, and the process is repeated.  (The other case is handled
    similarly.) The procedure continues until the width of the
    bracketting range gets smaller than \code{dx}.

    The process is fast, because every step reduces the search range by a
    factor of 2.
    
    Note that functions with multiple roots are not well-handled by
    \code{oceBisect} (or any bisection algorithm), because only one root will
    be found.

}

\value{A value of the independent variable that is within \code{eps} of a root.}

\examples{
hfxSunAngle <- function(t) sunAngle(t, lat=44+39/60, lon=-(63+36/60))$elevation
sunrise <- oceBisect(hfxSunAngle, 
                      as.numeric(as.POSIXct("2011-07-01 05:00:00", tz="UTC")), 
                      as.numeric(as.POSIXct("2011-07-01 10:00:00", tz="UTC")))
cat("Sunrise on Canada Day, 2011 is at", format(number.as.POSIXct(sunrise)), "UTC \n")
}

\author{Dan Kelley}

\keyword{misc}

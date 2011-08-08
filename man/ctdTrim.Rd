% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{ctdTrim}

\alias{ctdTrim}

\title{Trim start/end portions of a CTD cast}

\description{Trim start/end portions of a CTD cast.}

\usage{ctdTrim(x, method=c("downcast","index","range"), 
inferWaterDepth=TRUE, removeDepthInversions=FALSE,
parameters, debug=getOption("oceDebug"))}


\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{method}{Various methods exist, some of which use parameters:
    \describe{
      \item{"downcast"}{Select only data for which the CTD is descending.  This
	is done in stages.
	\enumerate{
	  \item{Step 1.}{The pressure data are despiked with a smooth()
	    filter with method "3R".  This removes wild spikes that arise
	    from poor instrument connections, etc.}
	  \item{Step 2.}{If no \code{parameters} are given, then any
	    data with negative pressures are deleted.  If there is a
	    parameter named \code{pmin}, then that pressure (in
	    decibars) is used instead as the lower limit.  This is a
	    commonly-used setup, e.g. \code{ctdTrim(ctd,
	    parameters=list(pmin=1))} removes the top decibar
	    (roughly 1m) from the data.}
	  \item{Step 3.}{The maximum pressure is determined, and data
	    acquired subsequent to that point are deleted.  This removes
	    the upcast and any subsequent data.}
	  \item{Step 4.}{An initial equilibrium phase is removed by a
	    regression of pressure on scan number.  The model has zero
	    pressure for some initial portion, and then a constant
	    increase with scan number.  Then this initial zero-pressure
	    portion is deleted.  (The regression may fail, and if so, a
	    warning is printed, and this step is skipped.)}
	}
      }

      \item{"index"}{Select values only in the list of indices specified in \code{parameters}.
        The indices may be integers, \emph{e.g.} \code{parameters=10:30} selects data points with
        indices 10, 11, ... 30, or logicals, \emph{e.g.} \code{parameters=c(TRUE,TRUE, ...)}.}

      \item{"range"}{Select data based on the value of the column named \code{parameters$item}.
        This may be by range or by critical value.  By range: select values between
        \code{parameters$from} (the lower limit) and \code{parameters$to} (the upper limit) By
        critical value: select if the named column exceeds the value.  For example, \code{ctd2 <-
          ctdTrim(ctd, "range", parameters=list(item="scan", from=5))} starts at scan number 5 and
        continues to the end, while
        \code{ctdTrim(ctd,"range",parameters=list(item="scan",from=5,to=100))} 	also starts at scan
        5, but extends only to scan 100.}


    }

  }

  \item{inferWaterDepth}{boolean, set to \code{TRUE} if the water depth
    (\code{metadata$water.depth}) is to be determined from the maximum of the trimmed pressure.
    (This is ignored if the original data file contained the water depth in the header.)}

  \item{removeDepthInversions}{Boolean value indicating whether to remove any levels at which depth
    is less than, or equal to, a depth above.  (This is needed if the object is to be assembled into
    a section, unless \code{\link{ctdDecimate}} will be used, which will remove the inversions.}

  \item{parameters}{A list whose elements depend on the method; see above.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount of debugging
    information, or to 2 to get more.} 

}

\details{For normal CTD profiling, the goal is to isolate only the
  downcast, discarding measurements made in the air, in an equilibration
  phase in which the device is held below the water surface, and then
  the upcast phase that follows the downcast.  This is handled
  reasonably well by \code{ctdTrim} with \code{method="downcast"}.
  (The datasets provided with \code{oce} were produced this way.)

  However, for detailed work it makes sense to do things semi-manually.
  The eye is simply better at handling exceptional cases.  The process
  is simple: use \code{plotScan()} to get an idea of the scan
  indices of the downacast, and then use \code{ctdTrim} with
  \code{method="index"}.  A few trials will normally identify the
  downcast very well.
}

\value{An object of \code{\link[base]{class}} \code{"ctd"}, with data having been trimmed in some way.}

\seealso{
  The \code{ctd} object may be read with \code{\link{read.ctd}}.  
  \code{\link{plotScan}} is very useful in providing guidance
  for trimming with \code{ctdTrim}.
}
\examples{
library(oce)
data(ctdRaw) 
plot(ctdRaw) # barely recognizable, due to pre- and post-cast junk
plot(ctdTrim(ctdRaw)) # looks like a real profile ...
plot(ctdDecimate(ctdTrim(ctdRaw),method="boxcar")) # ... smoothed
}

\references{The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}

\author{Dan Kelley}
\keyword{misc}

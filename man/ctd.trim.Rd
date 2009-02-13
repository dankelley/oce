\name{ctd.trim}
\alias{ctd.trim}
\title{Trim start/end portions of a CTD cast}
\description{Trim start/end portions of a CTD cast.}
\usage{ctd.trim(x, method="downcast",parameters, verbose=FALSE)}
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
	    commonly-used setup, e.g. \code{ctd2 <- ctd.trim(ctd,
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
      \item{"index"}{Select values only in indicated list of indices,
	\emph{e.g.} \code{selection <- ctd.trim(ctd, "index", seq(10,30))}
	selects data points 10, 11, ... 30.}
      \item{"(ANYTHING ELSE)"}{Select data only if the named item
	(\emph{e.g.} \code{scan}, \code{time}, \emph{etc}.) falls
	in the range of values indicated by the list stored in \code{parameters}.  
	This list has elements \code{from} (the lower limit) and \code{to}
	(the upper limit)  Sensible things happen if only one limit is
	given.  For example, \code{ctd2 <- ctd.trim(ctd, "scan", list(from=5))} 
	starts at scan number 5 and continues to the end,
	while \code{ctd3 <- ctd.trim(ctd, "scan", list(from=5,to=100))}
	also starts at scan 5, but extends only to scan 100.}
    }
  }
  \item{parameters}{A list whose elemetns depend on the method; see above.}
  \item{verbose}{If set to TRUE, some debugging information is provided.}
}

\details{For normal CTD profiling, the goal is to isolate only the
  downcast, discarding measurements made in the air, in an equilibration
  phase in which the device is held below the water surface, and then
  the upcast phase that follows the downcast.  This is handled
  reasonably well by \code{ctd.trim} with \code{method="downcast"}.
  (The datasets provided with \code{oce} were produced this way.)

  However, for detailed work it makes sense to do things semi-manually.
  The eye is simply better at handling exceptional cases.  The process
  is simple: use \code{plot.ctd.scan()} to get an idea of the scan
  indices of the downacast, and then use \code{ctd.trim} with
  \code{method="index"}.  A few trials will normally identify the
  downcast very well.
}

\value{An object of \code{\link[base]{class}} \code{"ctd"}, with data having been trimmed in some way.}

\seealso{
  The \code{ctd} object may be read with \code{\link{read.ctd}}.  
  \code{\link{plot.ctd.scan}} is very useful in providing guidance
  for trimming with \code{ctd.trim}.
}
\examples{
library(oce)
data(ctd.raw) 
plot(ctd.raw) # barely recognizable, due to pre- and post-cast junk
plot(ctd.trim(ctd.raw)) # looks like a real profile ...
plot(ctd.decimate(ctd.trim(ctd.raw),method="boxcar")) # ... smoothed
}

\references{The Seabird CTD instrument is described at
  \url{http://www.seabird.com/products/spec_sheets/19plusdata.htm}.
}

\author{Dan Kelley}
\keyword{misc}

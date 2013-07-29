% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{ctdFindDescents}

\alias{ctdFindDescents}

\title{Trim start/end portions of a CTD cast}

\description{Trim start/end portions of a CTD cast.}

\usage{
ctdFindDescents(x,
                dpdtCriterion=0.3,
                k=21,
                smallest=10,
                method=3,
                debug=getOption("oceDebug"))}

\arguments{
  \item{x}{A \code{ctd} object, e.g. as read by \code{\link{read.ctd}}.}
  \item{dpdtCriterion}{quantile fraction}
  \item{k}{\code{k} used in \code{\link{runmed}} for \code{method=2}}
  \item{smallest}{shortest descent candidates that are kept}
  \item{method}{1 for a simple method, 2 for a method using \code{\link{runmed}}, or 3 for a method using \code{\link{smooth}}.}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount of debugging information, or to 2 to get more.} 

}


\details{THIS IS A PRELIMINARY VERSION.  EVERYTHING ABOUT IT MAY CHANGE, INCLUDING THE ARGUMENTS.
  method=3 may be best.}

\value{A data frame with columns \code{start} and \code{end}, the indices of the downcasts.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
## Read data
d <- read.table("Edgeoya04.07csv.csv", sep=";", dec=",", header=TRUE)
salinity <- d[["Sal."]]
temperature <- d[["Temp"]] # or is it "T"?
pressure <- d[["Press"]]
scan <- d[["Meas"]]
datetime <- paste(d[["Date"]], d[["Time"]]) # 04.07.2013
time <- as.POSIXct(strptime(datetime, "%d.%m.%Y %H:%M:%S"))
c <- as.ctd(salinity, temperature, pressure, scan=scan)

## Find descents
indices <- ctdFindDescents(ctd)

## Plot
par(mfrow=c(1,1), mar=c(3,3,1,1), mgp=c(2, 0.75, 0))
oce.plot.ts(time, pressure)
abline(v=time[indices$start], col='red')
abline(v=time[indices$end], col='gray')
ncasts <- length(indices$start)
casts <- vector("list", ncasts)
for (i in 1:length(casts)) {
  ii <- seq.int(indices$start[i], indices$end[i])
  casts[[i]] <- as.ctd(salinity[ii], temperature[ii], pressure[ii])
}
par(mfrow=c(ncasts, 3))
for (c in casts) {
  plotProfile(c, "salinity")
  plotProfile(c, "temperature")
  plotTS(c, type='o')
}
}
}

\author{Dan Kelley}

\keyword{misc}

% vim:textwidth=100:expandtab:shiftwidth=2:softtabstop=2
\name{ctdFindProfiles}

\alias{ctdFindProfiles}

\title{Find profiles within a towyow CTD record}

\description{Examine the pressure record looking for extended periods of either ascent or descent,
  and return either indices to these events or a vector of CTD records, one for each.}

\usage{ctdFindProfiles(x, dpCriterion=0.3, k=21, smallest=10, method=3,
                arr.ind=FALSE, direction=c("descending", "ascending"),
                debug=getOption("oceDebug"))}

\arguments{
  \item{x}{A \code{ctd} object, as read by \code{\link{read.ctd}} or created by
    \code{\link{as.ctd}}.}
  \item{dpCriterion}{a quantile criterion for \code{diff(pressure)}}
  \item{k}{\code{k} used in \code{\link{runmed}} for \code{method=2}}
  \item{smallest}{shortest descent candidates that are kept}
  \item{method}{An integer that indicates the preferred method of isolating the ascents or descents,
    as described in \sQuote{Details}.}
  \item{arr.ind}{should array indices be returned, or a vector of ctd objects?}
  \item{direction}{string indicating the travel direction to be selected}
  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate amount of debugging
    information, or to 2 to get more.} 
}

\details{All the methods start by calculating \code{dp} as the first-difference of the pressure
  record.  If \code{method=1}, then \code{dp} values of the relevant sign (depending on
  \code{direction}) are examined, and values exceeding the \code{dpCriterion}-th quantile are
  consided to be within a profile.  If \code{method=2}, a running median is first run across
  \code{dp}, using \code{\link{runmed}} with filter length \code{k} as indicated by the argument to
  the present function.  If \code{method=3}, \code{\link{smooth}} is used instead of
  \code{\link{runmed}}.

  For all three methods, the next step is to remove any profiles that are of length shorter than
  \code{smallest}, as a way to prevent being misled by short diversions of the instrument from a
  path that is generally an ascent or descent.}

\value{If \code{arr.ind=TRUE}, a data frame with columns \code{start} and \code{end}, the indices of
  the downcasts.  Otherwise, a vector of \code{ctd} objects.}

\section{status}{This function was developed in July-August 2013, and needs more testing before it
  will be complete.  Since method=1 seems quite bad in test cases, it may be removed.  Indeed, it
  seems a good idea to have a new argument called \code{smoother} or something, and to use
  \code{...} arguments to give whatever that is some extra args.}

\seealso{The documentation for \code{\link{ctd-class}} explains the structure
    of CTD objects, and also outlines the other functions dealing with them.}

\examples{
\dontrun{
library(oce)
d <- read.csv("towyow.csv", header=TRUE)
towyow <- as.ctd(d$salinity, d$temperature, d$pressure)

casts <- ctdFindProfiles(towyow)
par(mfrow=c(length(casts), 3))
for (cast in casts) {
  plotProfile(cast, "salinity")
  plotProfile(cast, "temperature")
  plotTS(cast, type='o')
}
}
}

\author{Dan Kelley}

\keyword{misc}

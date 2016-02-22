% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2:spell
\name{oce-deprecated}
\alias{oce-deprecated}
\title{Deprecated functions in package \sQuote{oce}}

\description{Certain functions and function arguments are still provided for
  compatibility with older versions of \sQuote{oce}, but will be removed soon.
  The \sQuote{oce} scheme for removing functions is the same as that used by
  \sQuote{Bioconductor}: items are marked as "deprecated" in one release, as
  "defunct" in the next, and then removed entirely. This goal is to provide a
  gentle migration path for users who keep their packages reasonably
  up-to-date.}

\details{

  Several \sQuote{oce} functions are marked "deprecated" in the present release
  of oce. Please use the replacement functions as listed below.

  \tabular{lll}{
    \strong{Deprecated} \tab \strong{Replacement} \tab \strong{Notes}\cr
    \code{makeSection} \tab \code{\link{as.section}} \tab Improve utility and name sensibility\cr
    \code{mapZones} \tab \code{\link{mapGrid}} \tab Improve name sensibility\cr
    \code{mapMeridians} \tab \code{\link{mapGrid}} \tab Improve name sensibility\cr
  }

  The next CRAN release of \sQuote{oce} will have these functions flagged as
  "defunct", so trying to use them will generate an error.  Two CRAN releases
  from now, the functions will be removed entirely.  Please contact the
  developers if these plans will cause problems in your work.


  Several \sQuote{oce} function arguments are also slated for removal but are
  being permitted in the present CRAN release. They are as follows.

  \tabular{lll}{
    \strong{Deprecated} \tab \strong{Replacement} \tab \strong{Notes}\cr
    \code{fill} argument to \code{\link{mapPlot}} etc. \tab \code{col} \tab Rationalize with \code{\link{polygon}}\cr
  }
}

\seealso{The \sQuote{Bioconductor} scheme for removing functions is described at
  \url{https://www.bioconductor.org/developers/how-to/deprecation/} and it is
  extended here to function arguments.}


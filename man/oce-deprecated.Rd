% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2:spell
\name{oce-deprecated}
\alias{oce-deprecated}
\alias{oce-defunct}
\title{Deprecated functions and arguments in package \sQuote{oce}}

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
    \code{mapZones} \tab \code{\link{mapGrid}} \tab Improve name sensibility\cr
    \code{mapMeridians} \tab \code{\link{mapGrid}} \tab Improve name sensibility\cr
  }

  The next CRAN release of \sQuote{oce} will have these functions flagged as
  "defunct", which will mean that trying to use them will generate an error and
  a hint as to the replacement function.

  The following are marked "defunct", which means that calling them in the
  present version of oce will produce an error, and that they will be removed
  altogether in the next oce release on CRAN.

  \tabular{lll}{
    \strong{Defunct} \tab \strong{Replacement} \tab \strong{Notes}\cr
    \code{makeSection} \tab \code{\link{as.section}} \tab Improve utility and name sensibility\cr
    \code{columns} \tab \code{\link{read.ctd}} \tab Unnecessary, and never worked\cr
  }

  Several \sQuote{oce} function arguments are also slated for removal but are
  being permitted in the present CRAN release. They are as follows.

  \tabular{lll}{
    \strong{Deprecated} \tab \strong{Replacement} \tab \strong{Notes}\cr
    \code{date} argument to \code{\link{as.ctd}} \tab - \tab Was never used,
    and could be confused with \code{startTime}\cr
    \code{fill} argument to \code{\link{mapPlot}} etc. \tab \code{col} \tab Rationalize with \code{\link{polygon}}\cr
  }
}

\seealso{The \sQuote{Bioconductor} scheme for removing functions is described at
  \url{https://www.bioconductor.org/developers/how-to/deprecation/} and it is
  extended here to function arguments.}


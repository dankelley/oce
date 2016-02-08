% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2:spell
\name{oce-deprecated}
\alias{oce-deprecated}
\title{Deprecated functions in package \sQuote{oce}}

\description{These functions are provided for compatibility with older versions
  of \sQuote{oce}, but will be removed soon.  The \sQuote{oce} scheme for
  removing functions is the same as that used by \sQuote{Bioconductor}.}

\details{

  Several \sQuote{oce} functions are marked "deprecated" in the present release of
  oce; please use the replacement functions as listed below.

  \tabular{lll}{
    \strong{Deprecated} \tab \strong{Replacement} \tab \strong{Notes}\cr
    \code{makeSection} \tab \code{\link{as.section}} \tab Improve utility and name sensibility\cr
  }

  The next CRAN release of \sQuote{oce} will have these functions flagged as
  "defunct", so trying to use them will generate an error.

  The functions will be removed entirely, two CRAN releases from now.
 
  Please contact the developers if these plans will cause problems in your work.

}

\seealso{The \sQuote{Bioconductor} scheme for removing functions is described at
  \url{https://www.bioconductor.org/developers/how-to/deprecation/}}

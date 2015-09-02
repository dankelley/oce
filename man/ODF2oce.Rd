% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{ODF2oce}

\alias{ODF2oce}

\title{Create odf object from the output of \code{ODF::read_ODF()}}

\description{As of August 11, 2015, \code{ODF::read_ODF} returns a list with 9
  elements, one named \code{DATA}, which is a \code{\link{data.frame}}
  containing the columnar data, the others being headers of various sorts.  The
  present function constructs an oce object from such data, facilitating
  processing and plotting with the general oce functions.}

\usage{ODF2oce(ODF, coerce=TRUE, debug=getOption("oceDebug"))}

\arguments{

  \item{ODF}{A list as returned by \code{read_ODF} in the \code{ODF} package}

  \item{coerce}{A logical value indicating whether to coerce the return value to
    an appropriate object type, if possible.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
    amount of debugging information, or to 2 to get more.}

}

\details{This function works by storing the 8 headers verbatim in the
  \code{odfHeaders} in the \code{metadata} slot. It copies some of the header
  information into more standard names (e.g.  \code{metadata@longitude} is a
  copy of \code{metadata@odfHeader$EVENT_HEADER$INITIAL_LATITUDE}).  As for the
  \code{DATA}, they are stored in the \code{data} slot, after renaming from ODF
  to oce convention using \code{\link{ODFNames2oceNames}}.}

\section{Caution}{This function may change as the \code{ODF} package changes.
  Since \code{ODF} has not been released yet, this should not affect any users
  except those involved in the development of \code{oce} and \code{ODF}.} 

\value{An oce object, possibly coerced to a subtype.}
    
\author{Dan Kelley}

\keyword{misc}


\name{lisst-class}

\docType{class}

\alias{lisst-class}
\alias{[[,lisst-method}
\alias{[[<-,lisst-method}
\alias{show,lisst-method}

\title{Class to store LISST data}

\description{Class to store LISST data.}

\section{Methods}{

    The sections below assume a LISST object named \code{d}.  Generally, these
    are created by \code{\link{as.lisst}} or by \code{\link{read.lisst}}, and
    the documentation for the latter should be consulted for information on the
    object contents.

    \subsection{Extracting values}{The time-series data may be accessed as e.g.
        \code{d[["c1"]]} for size-class numbrer 1.  There are 32 size classes.
        Also available are \code{d[["temperature"]]} and \code{d[["pressure"]]}
        and \code{d[["time"]]}.  See \code{\link{read.lisst}} for further
        details.  Items in \code{metadata} must be specifield by full name, but
        those in \code{data} may be abbreviated, so long as the abbreviation is
        unique.  }

    \subsection{Assigning values}{This follows the standard form, e.g. to
        smooth data in size class 1 over time, do
        \code{d[["c1"]] <- smooth(d[["c1"]]}.}

    \subsection{Overview of contents}{The \code{show} method (e.g.
        \code{show(d)}) displays information about a lisst object named
        \code{d}.}
}

\references{A users's manual for the LISST-100 instrument is available at the
    manufacturer's website \url{http://www.sequoiasci.com}.}

\seealso{One may read lisst objets with \code{\link{read.lisst}}, plot them
    with \code{\link{plot.lisst}}, and summarize them with
    \code{\link{summary}}.}
    
\author{Dan Kelley}

\keyword{classes}

\keyword{oce}


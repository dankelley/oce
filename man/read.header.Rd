\name{read.header}
\alias{read.header.nortek}

\title{Read an instrument header}

\description{Read an instrument header}

\usage{read.header.nortek(file, debug=getOption("oce.debug"))}

\arguments{
  \item{file}{a connection or a character string giving the name of the
    file to load.}
  \item{debug}{a Boolean, set to \code{TRUE} to debug the reading process.}
}

\details{Used by \code{\link{read.adp.nortek}} and \code{link{read.adv.nortek}}.}


\author{Dan Kelley}

\keyword{misc}

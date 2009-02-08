\name{ctd}

\docType{data}

\alias{ctd}

\title{Sample seawater CTD profile}

\description{This is sample CTD profile provided for testing, created by
  \code{ctd <- ctd.trim(read.oce("../tests/ctd.cnv"))} and then
  \code{save(ctd, file="../data/ctd.rda")}.  Note that the
  \code{\link{ctd.trim}} is not actually needed, because this particular
  data file has already been pruned by selecting only the portion of the
  data acquired when the instrument was being lowered through the water
  column.}

\usage{data(ctd)}

\format{This file is from a \emph{Sea-Bird SBE 19} CTD instrument.}

\source{Peter Galbraith acquired these data during the 2002 SLEIWEX
  field program, part of a Dalhousie/IML cooperative study of
  internal-wave mixing in the St Lawrence estuary.}

\seealso{See \code{\link{read.ctd}} for the data format.}

\author{Dan Kelley}

\keyword{misc}

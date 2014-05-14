\name{rangeExtended}

\alias{rangeExtended}

\title{Calculate range, extended a little}

\description{Calculate range, extended a little, as is done for axes.}

\usage{rangeExtended(x, extend=0.04)}

\arguments{
    \item{x}{a numeric vector.}
    \item{extend}{fraction to extend on either end}
}

\details{This is analogous to what is done as part of the R axis range
    calculation, in the case where \code{xaxs="r"}.}

\value{A two-element vector with the extended range of \code{x}.}

\author{Dan Kelley}

\keyword{misc}

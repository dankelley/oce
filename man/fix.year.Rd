\name{unabbreviate.year}

\alias{unabbreviate.year}

\title{Determine year from various abbreviations}

\description{Determine year from various abbreviations}

\usage{unabbreviate.year(year)}

\arguments{
  \item{year}{a year, or vector of years, possibly abbreviated}
}

\details{Various data files may contain various abbreviations for
  years.  For example, 99 refers to 1999, and 8 refers to 2008.
  Sometimes, even 108 refers to 2008 (the idea being that the "zero"
  year was 1900).  This function deals with the three cases mentioned.
  It will fail if someone supplies 60, meaning year 2060 as opposed to
  1960.}

\examples{
full.year <- unabbreviate.year(c(99, 8, 108))
}

\author{Dan Kelley}

\keyword{misc}

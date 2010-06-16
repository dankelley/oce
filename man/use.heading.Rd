\name{use.heading}

\alias{use.heading}

\title{Replace the heading for one instrument with the heading for
  another}

\description{Replace the heading for one instrument with the heading for
  another.}

\usage{use.heading(from, to, add=0)}

\arguments{
  \item{from}{object holding data from an instrument whose heading is
    not useful.}
  \item{to}{object holding data from an instrument whose heading is
    useful.}
  \item{add}{an angle, in degrees counterclockwise, to be added to the
    \code{to} heading.}
}

\value{A copy of \code{from}, but with its \code{$data$ts$heading} as an
  angle that result from linear interpolation of the headings in
  \code{from} after adding \code{add}.}

\author{Dan Kelley}

\keyword{misc}

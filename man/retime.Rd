\name{retime}

\alias{retime}

\title{Retime an Oce object}

\description{Retime an Oce object}

\usage{retime(x,a,b,t0,debug=getOption("oce.debug"))}

\arguments{
    \item{x}{an \code{oce} object (presently, only works for type
            \code{adv})}
    \item{a}{intercept in linear model of time drift (see
            \dQuote{Details}).}
    \item{b}{slope in linear model of time drift (see \dQuote{Details}).}
    \item{t0}{a POSIXct time offset in linear model of time drift (see
            \dQuote{Details}).}
    \item{debug}{a flag that, if nonzero, turns on debugging.}
}

\details{The new time is defined by \eqn{a + b * (t-t0)}{a + b*(t-t0)}
where \eqn{t}{t} is the time in the object.  For example, a simple
time offset has a non-zero value of \code{a}, a zero value of
\code{b}, and an arbitrary value of \code{t0}.

The retimed object is computed by linear interpolation, using
\code{\link{approx}} with \code{rule=2}, to avoid \code{NA} values at
the start or end.  The new time will be as given by the formula above.
Note that if the drift is large enough, the sampling rate will be
changed.  It is a good idea to start with an object that has an
extended time range, so that, after this is called,
\code{\link{subset.oce}} can be used to trim to a desired time range.

Hint: match the timezone of \code{t0} to the data, e.g.  with
\code{tz="UTC"} for both.}

\value{Retimed object.}

\examples{
\dontrun{
load("/data/archive/sleiwex/2008/moorings/m08/adv/sontek_g157/r/m08_sontek_xyz.rda")
d <- subset(m08.adv.xyz, as.POSIXct("2008-06-27 00:00:00",tz="UTC") <= time 
        & time <= as.POSIXct("2008-06-27 10:00:00",tz="UTC"))
dd <- retime(d, 0, 1e-4, as.POSIXct("2008-07-01 00:00:00", tz="UTC"))
}
}

\author{Dan Kelley}

\keyword{misc}

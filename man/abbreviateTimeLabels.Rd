\name{abbreviateTimeLabels}
\alias{abbreviateTimeLabels}
\title{Abbreviate a list of times by removing commonalities (e.g. year)}
\description{Abbreviate a list of times by removing commonalities (e.g. year)}
\usage{abbreviateTimeLabels(t, \dots)
}
\arguments{
  \item{t}{vector of times.}
  \item{\dots}{optional arguments passed to the \code{\link{format}}, e.g. \code{format}.}
}

\details{Using \code{\link{format}} A list FormatExamines the character list Finds common
  Creates an image with a colour palette to the right.  The effect is similar
  to \code{\link{filled.contour}} except that with \code{imagep} it is possible
  to set the \code{\link{layout}} outside the function, which enables the
  creation of plots with many image-palette panels.  Note that the contour
  lines may not coincide with the colour transitions, in the case of coarse
  images.

  Note that this does not use \code{\link{layout}} or any of the other screen
  splitting methods.  It simply manipulates margins, and draws two plots
  together.  This lets users employ their favourite layout schemes.

  The palette is drawn before the image, so that further drawing can be done on
  the image if desired, if the user prefers not to use the \code{adorn}
  argument.

  NOTE: \code{imagep} is an analogue of \code{\link{image}}, and so it borrows
  a somewhat odd convention: the number of rows in the matrix corresponds to
  the \code{x} axis, not the \code{y} axis.  (Actually, \code{\link{image}}
  permits the length of \code{x} to match either \code{nrow(z)} or
  \code{1+nrow(z)}, but here only the first is permitted.)

}

\value{None.}

\seealso{
  This is used by \code{\link{plot.adp}}.
}
\examples{
library(oce)

## simplest use
imagep(volcano)

## something oceanographic (internal-wave speed)
par(mfrow=c(1,1))
h <- seq(0, 50, length.out=200)
drho <- seq(1, 3, length.out=200)
speed <- outer(drho, h, function(drho, h) sqrt(9.8 * drho * h / 1024))
imagep(h, drho, speed, xlab="Equivalent depth [m]",
  ylab=expression(paste(Delta*rho, " [kg/m^3]")),
  zlab="Internal-wave speed [m/s]")

## fancy labelling on atan() function
x <- seq(0, 1, 0.01)
y <- seq(0, 1, 0.01)
angle <- outer(x,y,function(x,y) atan2(y,x))
imagep(x, y, angle, filledContour=TRUE, breaks=c(0, pi/4, pi/2),
  col=c("lightgray", "darkgray"),
  at=c(0, pi/4, pi/2),
  labels=c(0, expression(pi/4), expression(pi/2)))
}
\author{Dan Kelley, with help from Clark Richards}
\keyword{misc}

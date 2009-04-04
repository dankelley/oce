% Based on file src/library/graphics/man/screen.Rd
% Part of the R package, http://www.R-project.org
% Copyright 1995-2007 R Core Development Team
% Distributed under GPL 2 or later
% (The single difference is to rename quantities, to avoid a name clash.)
\name{oce.screen}
\title{Creating and Controlling Multiple Screens on a Single Device}
\usage{
oce.split.screen(figs, screen, erase = TRUE)
oce.screen(n = , new = TRUE)
oce.erase.screen(n = )
oce.close.screen(n, all.screens = FALSE)
}
\alias{oce.screen}
\alias{oce.split.screen}
\alias{oce.erase.screen}
\alias{oce.close.screen}
\arguments{
  \item{figs}{A two-element vector describing the number of rows and the
    number of columns in a screen matrix \emph{or} a matrix with 4 columns.
    If a matrix, then each row describes a screen with values for the left,
    right, bottom, and top of the screen (in that order) in NDC units,
    that is 0 at the lower left corner of the device surface, and 1 at
    the upper right corner.}
  \item{screen}{A number giving the screen to be split.  It defaults to
    the current screen if there is one, otherwise the whole device region.}
  \item{erase}{logical: should be selected screen be cleared?}
  \item{n}{A number indicating which screen to prepare for drawing
    (\code{oce.screen}), erase (\code{oce.erase.screen}), or close
    (\code{oce.close.screen}).  (\code{oce.close.screen} will accept a vector of
    screen numbers.)}
  \item{new}{A logical value indicating whether the screen should be
    erased as part of the preparation for drawing in the screen.}
  \item{all.screens}{A logical value indicating whether all of the
    screens should be closed.}
}
\description{
  \code{oce.split.screen} and the related functions are no more than a
  repackaging of the standard \code{\link[graphics]{split.screen}}, but with an
  alteration that otherwise prevents building the \code{oce} package.
  (The problem is that the \code{\link[graphics]{split.screen}} setup stores an
  item \code{mfg} that causes a build error with \R version 2.8.x.)

  \code{oce.split.screen} defines a number of regions within the current
  device which can, to some extent, be treated as separate graphics
  devices.  It is useful for generating multiple plots on a single
  device.  Screens can themselves be split, allowing for quite complex
  arrangements of plots.

  \code{oce.screen} is used to select which screen to draw in.

  \code{oce.erase.screen} is used to clear a single screen, which it
  does by filling with the background colour.

  \code{oce.close.screen} removes the specified screen definition(s).
}
\details{
  The first call to \code{\link[graphics]{split.screen}} places \R into split-screen
  mode.  The other split-screen functions only work within this mode.
  While in this mode, certain other commands should be avoided (see the
  Warnings section below).  Split-screen mode is exited by the command
  \code{oce.close.screen(all = TRUE)}.

  If the current screen is closed, \code{oce.close.screen} sets the current
  screen to be the next larger screen number if there is one, otherwise
  to the first available screen.
}
\value{
  \code{oce.split.screen} returns a vector of screen numbers for the
  newly-created screens.  With no arguments, \code{oce.split.screen} returns
  a vector of valid screen numbers.

  \code{oce.screen} invisibly returns the number of the selected screen.
  With no arguments, \code{oce.screen} returns the number of the current
  screen.

  \code{oce.close.screen} returns a vector of valid screen numbers.

  \code{oce.screen}, \code{oce.erase.screen}, and \code{oce.close.screen} all return
  \code{FALSE} if \R is not in split-screen mode.
}

\section{Warnings}{
  The recommended way to use these functions is to completely draw a
  plot and all additions (i.e. points and lines) to the base plot, prior to
  selecting and plotting on another screen. The behavior associated with
  returning to a screen to add to an existing plot is unpredictable and
  may result in problems that are not readily visible.

  These functions are totally incompatible with the other mechanisms for
  arranging plots on a device: \code{\link{par}(mfrow)},
  \code{par(mfcol)} and \code{\link{layout}()}.

  The functions are also incompatible with some plotting functions,
  such as \code{\link{coplot}}, which make use of these other mechanisms.

  \code{oce.erase.screen} will appear not to work if the background colour
  is transparent (as it is by default on most devices).
}

\seealso{
  \code{\link{par}}, \code{\link{layout}}, \code{\link{Devices}},
  \code{dev.*}
}
\references{
  Chambers, J. M. and Hastie, T. J. (1992)
  \emph{Statistical Models in S}.
  Wadsworth \& Brooks/Cole.

  Murrell, P. (2005) \emph{R Graphics}. Chapman & Hall/CRC Press.
}

\examples{
if (interactive()) {
par(bg = "white")           # default is likely to be transparent
oce.split.screen(c(2,1))        # split display into two screens
oce.split.screen(c(1,3), screen = 2) # now split the bottom half into 3
oce.screen(1) # prepare screen 1 for output
plot(10:1)
oce.screen(4) # prepare screen 4 for output
plot(10:1)
oce.close.screen(all = TRUE)    # exit split-screen mode

oce.split.screen(c(2,1))        # split display into two screens
oce.split.screen(c(1,2),2)      # split bottom half in two
plot(1:10)                  # screen 3 is active, draw plot
oce.erase.screen()              # forgot label, erase and redraw
plot(1:10, ylab= "ylab 3")
oce.screen(1)                   # prepare screen 1 for output
plot(1:10)
oce.screen(4)                   # prepare screen 4 for output
plot(1:10, ylab="ylab 4")
oce.screen(1, FALSE)            # return to screen 1, but do not clear
plot(10:1, axes=FALSE, lty=2, ylab="")  # overlay second plot
axis(4)                     # add tic marks to right-hand axis
title("Plot 1")
oce.close.screen(all = TRUE)    # exit split-screen mode
}
}
\keyword{aplot}
\keyword{dplot}
\keyword{device}

\name{plot.echosounder}

\alias{plot.echosounder}
\alias{plot,echosounder,missing-method}
\alias{plot,echosounder-method}

\title{Plot echosounder data}

\description{Plot echosounder data}

\usage{\S4method{plot}{echosounder}(x,
     which=1, 
     newx, xlab="",
     zlim,
     type="l", col="black", lwd=2,
     despike=FALSE, drawBottom,
     adorn=NULL,
     mgp=getOption("oceMgp"),
     mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
     debug=getOption("oceDebug"),
     \dots)
}
\arguments{

  \item{x}{An \code{echosounder} object, e.g. as read by
      \code{\link{read.echosounder}}, or created by
      \code{\link{as.echosounder}}.}
  \item{which}{list of desired plot types.  
    \itemize{
      \item \code{which=1} gives a depth-time image
      \item \code{which=2} gives a depth-distance image
      \item \code{which=3} gives a map showing the cruise track
    }
  }

  \item{newx}{optional vector of values to appear on the horizontal axis if
      \code{which=1}, instead of time.  This must be of the same length as the
      time vector, because the image is remapped from time to \code{newx} using
      \code{\link{approx}}.}

  \item{xlab}{label for horizontal axis.}

  \item{zlim}{optional range for colorscale.}

  \item{type}{type of graph, \code{"l"} for line, \code{"p"} for points, or
      \code{"b"} for both.}
  \item{col}{color for line, or points}
  \item{lwd}{line width (ignored if \code{type="p"})}

  \item{despike}{remove vertical banding by using \code{\link{smooth}} to smooth
      across image columns, row by row.}

  \item{drawBottom}{optional flag used for section images.  If \code{TRUE},
      then the bottom is inferred as a smoothed version of the ridge of highest
      image value, and data below that are whited out after the image is drawn.
      If \code{drawBottom} is a colour, then that colour is used, instead of
      white.}
      
  \item{adorn}{list of expressions to be executed for the panels in turn, e.g.
      to adorn the plots.  If the number matches the number of panels, then the
      strings are applied to the appropriate panels, as they are drawn from
      top-left to bottom-right.   If only a single expression is provided, it
      is used for all panels.  (See \dQuote{Examples}.)} \item{mgp}{3-element
      numerical vector to use for \code{par(mgp)}, and also for
      \code{par(mar)}, computed from this.  The default is tighter than the R
      default, in order to use more space for the data and less for the axes.}
  \item{mar}{value to be used with \code{\link{par}("mar")}.} \item{debug}{set
      to an integer exceeding zero, to get debugging information during
      processing.} \item{\dots}{optional arguments passed to plotting
      functions.} }

\details{FIXME: add more here}

\seealso{The documentation for \code{\link{echosounder-class}} explains the
    structure of echosounder objects, and also outlines the other functions
    dealing with them.}

\examples{
library(oce)
\dontrun{
e <- read.echosounder("myfile.dt4")
plot(echosounder)
plot(echosounder, drawBottom="gray")
plot(echosounder, which=3) # map
}
}

\author{Dan Kelley}

\keyword{hplot}

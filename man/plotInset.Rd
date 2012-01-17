\name{plotInset}

\alias{plotInset}

\title{Plot an inset diagram}

\description{Plot an inset diagram}


\usage{plotInset(xleft, ybottom, xright, ytop, expr,
    bg="white", fg="black", mar=c(2, 2, 1, 1),
    debug=getOption("oceDebug"))}

\arguments{

  \item{xleft}{location of left-hand of the inset diagram, in the existing plot
      units.}
  
  \item{ybottom}{location of bottom side of the inset diagram, in the existing
      plot units.}

  \item{xright}{location of right-hand side of the inset diagram, in the
      existing plot units.}

  \item{ytop}{location of top side of the inset diagram, in the existing plot
      units.}

  \item{expr}{An expression that draws the inset plot.}

  \item{bg}{colour of the background, to be painted under the inset.  (Try e.g.
      \code{"yellow"} to debug the margins as set by \code{mar}.)}

  \item{fg}{colour for the box enclosing the inset diagram}

  \item{mar}{margins, in line heights, to be used at the four sides of the
      inset diagram.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.} 
}
  

\details{Adds an inset diagram to an existing plot.  Note that if the inset is
    a map or coastline, it will be necessary to supply \code{mar=NULL} to
    prevent the inset diagram from occupying the whole device width.}

\section{Notes}{This is a provisional function that may be removed, or that act
    differently, until it is tested on several real-world cases.  It should be
    used with caution.}

\examples{
library(oce)
data(ctd) 
plot(ctd, which="TS")
plotInset(29.9, 2.8, 31.1, 9,
          expr=plot(ctd, which='map', coastline="coastlineMaritimes", span=250, mar=NULL, cex.axis=3/4))
}
\author{Dan Kelley}
\keyword{hplot}

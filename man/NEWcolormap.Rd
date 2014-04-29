\name{Colormap}

\alias{Colormap}

\title{Calculate color map (Draft Version)}

\description{Map values to colors, for use in palettes and plots.}

\usage{Colormap(z,
         breaks, col,
         name,
         x0, x1, col0, col1,
         n=1,
         missingColor,
         debug=getOption("oceDebug"))}

\arguments{

  \item{z}{an optional vector or other set of numerical values to be examined.
      If \code{z} is given, the return value will contain an item named
      \code{zcol} that will be a vector of the same length as \code{z},
      containing a color for each point.  If \code{z} is not given,
      \code{zcol} will contain just one item, the color \code{"black"}.}

  \item{breaks}{an optional indication of break points between color levels
      (see \code{\link{image}}).  If this is provided, the arguments
      \code{name} through \code{n} are all ignored (see \dQuote{Details}).  If
      it is provided, then it may either be a vector of break points, or a
      single number indicating the desired number of break points to be
      computed with \code{\link{pretty}(z, breaks)}.  In either case of
      non-missing \code{breaks}, the resultant break points must number 1 plus
      the number of colors (see \code{col}).}
      
  \item{col}{either a vector of colors or a function taking a numerical value
      as its single argument and returning a vector of colors.  The value of
      \code{col} is ignored if \code{name} is provided, or if \code{x0} through
      \code{col1} are provided.}

  \item{name}{an optional string naming a built-in colormap (one of
      \code{"gmt_relief"}, \code{"gmt_ocean"}, \code{"gmt_globe"} or
      \code{"gmt_gebco"}) or the name of a file or URL that contains a color
      map specification in GMT format, e.g.
      \code{"http://www.beamreach.org/maps/gmt/share/cpt/GMT_globe.cpt"}). If
      \code{name} is provided, then \code{x0}, \code{x1}, \code{col0} and
      \code{col1} are all ignored.}

  \item{x0, x1, col0, col1}{Vectors that specify a color map.  They must all be
      the same length, with \code{x0} and \code{x1} being numerical values, and
      \code{col0} and \code{col1} being colors.  The colors may be strings
      (e.g. \code{"red"}) or colors as defined by \code{\link{rgb}} or
      \code{\link{hsv}}.  }
  
  \item{n}{number of sub-intervals to use between successive values of
      \code{x0}, etc.  This may be a single number, or a vector of numbers of
      length one less than the length of \code{x0}.}

  \item{missingColor}{color to use for missing values.  This will be set
      automatically if \code{name} is used to specify the color mapping.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
      moderate amount of debugging information, or to 2 to get more.} 

} 

\details{This is a multi-purpose function that generally links (``maps'')
    numerical values to colors.  The return value can specify colors for points
    on a graph, or \code{breaks} and \code{col} vectors that are suitable for
    use by \code{\link{drawPalette}}, \code{\link{imagep}} or
    \code{\link{image}}.
    
    There are three ways of specifying color schemes, and \code{Colormap} works
    by checking for each condition in turn.
    \itemize{
        
        \item\emph{Case A.} Supply \code{z} but nothing else.  In this case,
        \code{breaks} will be set to \code{\link{pretty}(z, 10)} and things are
        otherwise as in case 2.

        \item\emph{Case B.} Supply \code{breaks}.  In this case, \code{breaks}
        and \code{col} are used together to specify a color scheme.  If
        \code{col} is a function, then it is expected to take a single
        numerical argument that specifies the number of colors, and this number
        will be set to \code{length(breaks)-1}.  Otherwise, \code{col} may be a
        vector of colors, and its length must be one less than the number of
        breaks.  \emph{FIXME: discuss existence of z here}  (NB. if
        \code{breaks} is given, then all other arguments except \code{col} and
        \code{missingColor} are ignored.)

        \item\emph{Case C.} Do not supply \code{breaks}, but supply \code{name}
        instead.  This \code{name} may be the name of a pre-defined color
        palette (\code{"gmt_relief"}, \code{"gmt_ocean"}, \code{"gmt_globe"} or
        \code{"gmt_gebco"}), or it may be the name of a file (including a URL)
        containing a color map in the GMT format (see \dQuote{References}).
        (NB. if \code{name} is given, then all other arguments except
        \code{missingColor} are ignored.)
        
        \item\emph{Case D.} Do not supply either \code{breaks} or \code{name},
        but instead supply each of \code{x0}, \code{x1}, \code{col0}, and
        \code{col1}.  These values are specify a value-color mapping that is
        similar to that used for GMT color maps.  The method works by using
        \code{\link{seq}} to interpolate between the elements of the \code{x0}
        vector.  (FIXME state more clearly) The same is done for \code{x1}.
        Similarly, \code{\link{colorRampPalette}} is used to interpolate
        between the colors in the \code{col0} vector, and the same is done for
        \code{col1}.  (FIXME: explain more, perhaps by repeating a statement
        about how GMT is done.) In this way, a colormap can be constructed by
        specifying a few points.
    }

}
 
\value{A list containing the following (not necessarily in this order) 
    \itemize{

        \item \code{zcol}, a vector of colors for \code{z}, if \code{z} was
        provided, otherwise \code{"black"}

        \item \code{zlim}, a two-element vector
        suitable as the argument of the same name supplied to \code{\link{image}}
        or \code{\link{imagep}}

        \item \code{breaks} and \code{col}, vectors of breakpoints and colors,
        suitable as the same-named arguments to \code{\link{image}} or
        \code{\link{imagep}}

        \item \code{x0} and \code{x1}, numerical vectors of the sides of color
        intervals, and \code{col0} and \code{col1}, vectors of colors between
        which those intervals blend.  The intervals are each decomposed into
        \code{n} sub-intervals.
        
        \item \code{missingColor}, a color that could be used to specify
        missing values, e.g. as the same-named argument to
        \code{\link{imagep}}.  If this is supplied as an argument, its value is
        repeated in the return value.  Otherwise, its value is either
        \code{"gray"} or, in the case of \code{name} being given, the value in
        the GMT color map specification.

    }

    Unless the user wishes to alter a color mapping, the last four of these are
    not needed (but see example 3 in \dQuote{Examples}).

}

\section{Development status}{Devised in early May 2014, this function is likely
    to be somewhat unstable until mid-summer 2014, with changes to the argument
    list and return value.
}
    
\examples{
\dontrun{
library(oce)

## Example 1. color scheme for points on xy plot
x <- seq(0, 1, length.out=40)
y <- sin(2 * pi * x)
par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
mar <- par('mar') # prevent margin creep by drawPalette()
## Top panel: default breaks
c <- Colormap(y)
drawPalette(c$zlim, col=c$col, breaks=c$breaks)
plot(x, y, bg=c$zcol, pch=21, cex=1)
grid()
par(mar=mar)
## Bottom panel: 100 breaks, yielding a smoother palette
c <- Colormap(y, breaks=100)
drawPalette(c$zlim, col=c$col, breaks=c$breaks)
plot(x, y, bg=c$zcol, pch=21, cex=1)
grid()
par(mar=mar)

## Example 2. image with a standard color scheme
par(mfrow=c(1,1))
data(topoWorld)
cm <- Colormap(name="gmt_globe")
imagep(topoWorld, breaks=cm$breaks, col=cm$col)

## Example 3. image with a modified color scheme
cm <- Colormap(name="gmt_globe")
deep <- cm$x0 <= -4000
cm$col0[deep] <- 'black'
cm$col1[deep] <- 'black'
cm <- Colormap(x0=cm$x0, x1=cm$x1, col0=cm$col0, col1=cm$col1)
imagep(topoWorld, breaks=cm$breaks, col=cm$col)

## Example 4. construct color map
## FIXME: something wrong here
cm <- Colormap(x0=c(-8000, -4000, 0, 100),
               x1=c(-8000, -4000, 0, 100),
               col0=c("violet","blue","white","tan"),
               col1=c("violet","blue","white","tan"),
               n=c(10, 10, 1))
imagep(topoWorld, breaks=cm$breaks, col=cm$col)
}
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}


\name{colormap}

\alias{colormap}

\title{Calculate color map}

\description{Map values to colors, for use in palettes and plots.}

\usage{colormap(z,
         zlim, zclip=FALSE,
         breaks, col=oce.colorsJet,
         name, x0, x1, col0, col1, blend=0,
         missingColor,
         debug=getOption("oceDebug"))}

\arguments{

  \item{z}{an optional vector or other set of numerical values to be examined.
      If \code{z} is given, the return value will contain an item named
      \code{zcol} that will be a vector of the same length as \code{z},
      containing a color for each point.  If \code{z} is not given,
      \code{zcol} will contain just one item, the color \code{"black"}.}

  \item{zlim}{optional vector containing two numbers that specify the \code{z}
      limits for the colorscale.  If not given, this will be determined from
      the other arguments, as follows.  If \code{name} is given, then the
      \code{\link{range}} of numerical values contained therein will be
      used for \code{zlim}.  Otherwise, if \code{z} is given, then its
      \code{\link{rangeExtended}} sets \code{zlim}.  Otherwise, if \code{x0}
      and \code{x1} are given, then their \code{\link{range}} sets
      \code{zlim}.  Otherwise, there is no way to infer \code{zlim} and indeed
      there is no way to construct a colormap, so an error is reported.  It is
      an error to specify both \code{zlim} and \code{breaks}, if the length of
      the latter does not equal 1.}

  \item{zclip}{logical, indicating whether to clip the colors to those
      corresponding to \code{zlim}, if the latter is provided.  Clipped regions
      will be colored with \code{missingColor}.}

  \item{breaks}{an optional indication of break points between color levels
      (see \code{\link{image}}).  If this is provided, the arguments
      \code{name} through \code{blend} are all ignored (see \dQuote{Details}).
      If it is provided, then it may either be a vector of break points, or a
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
      map specification in GMT format, e.g. one of the \code{.cpt} files from 
      \url{http://www.beamreach.org/maps/gmt/share/cpt}). If
      \code{name} is provided, then \code{x0}, \code{x1}, \code{col0} and
      \code{col1} are all ignored.}

  \item{x0, x1, col0, col1}{Vectors that specify a color map.  They must all be
      the same length, with \code{x0} and \code{x1} being numerical values, and
      \code{col0} and \code{col1} being colors.  The colors may be strings
      (e.g. \code{"red"}) or colors as defined by \code{\link{rgb}} or
      \code{\link{hsv}}.  }
  
  \item{blend}{a number indicating how to blend colors within each band.  This
      is ignored except when \code{x0} through \code{col1} are supplied.  A
      value of 0 means to use \code{col0[i]} through the interval \code{x0[i]}
      to \code{x1[i]}.   A value of 1 means to use \code{col1[i]} in that
      interval.  A value between 0 and 1 means to blend between the two colors
      according to the stated fraction.  Values exceeding 1 mean to break up
      the domain into \code{blend} sub-intervals; making this a large number
      yields a palette that blends smoothly between the fixed colors specified
      in \code{col0} and \code{col1}.}

  \item{missingColor}{color to use for missing values.  If not provided, this
      will be \code{"gray"}, unless \code{name} is given, in which case it
      comes from that color table.}

  \item{debug}{a flag that turns on debugging.  Set to 1 to get a
      moderate amount of debugging information, or to 2 to get more.} 

} 

\details{This is a multi-purpose function that generally links (``maps'')
    numerical values to colors.  The return value can specify colors for points
    on a graph, or \code{breaks} and \code{col} vectors that are suitable for
    use by \code{\link{drawPalette}}, \code{\link{imagep}} or
    \code{\link{image}}.
    
    There are three ways of specifying color schemes, and \code{colormap} works
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
        breaks.  (NB. if \code{breaks} is given, then all other arguments
        except \code{col} and \code{missingColor} are ignored.) \if{html}{The
            figure below explains the (\code{breaks}, \code{col}) method of
            specifying a color mapping.  Note that there must be one more break
            than color.  This is the method used by e.g.  \code{\link{image}}.

            \figure{colormap_fig_1.png}{options: width=400px alt="Figure: colormap\_fig\_1.png"}}
 
        \item\emph{Case C.} Do not supply \code{breaks}, but supply \code{name}
        instead.  This \code{name} may be the name of a pre-defined color
        palette (\code{"gmt_relief"}, \code{"gmt_ocean"}, \code{"gmt_globe"} or
        \code{"gmt_gebco"}), or it may be the name of a file (including a URL)
        containing a color map in the GMT format (see \dQuote{References}).
        (NB. if \code{name} is given, then all other arguments except
        \code{z} and \code{missingColor} are ignored.)
       
        \item\emph{Case D.} Do not supply either \code{breaks} or \code{name},
        but instead supply each of \code{x0}, \code{x1}, \code{col0}, and
        \code{col1}.  These values are specify a value-color mapping that is
        similar to that used for GMT color maps.  The method works by using
        \code{\link{seq}} to interpolate between the elements of the \code{x0}
        vector.  The same is done for \code{x1}.  Similarly,
        \code{\link{colorRampPalette}} is used to interpolate between the
        colors in the \code{col0} vector, and the same is done for \code{col1}.
        \if{html}{The figure below explains the (\code{x0}, \code{x1},
            \code{col0}, \code{col1}) method of specifying a color mapping.
            Note that the each of the items has the same length. The case of
            \code{blend=0}, which has color \code{col0[i]} between
            \code{x0[i]} and \code{x1[i]}, is illustrated below.

            \figure{colormap_fig_2.png}{options: width=400px alt="Figure: colormap\_fig\_2.png"}}
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

        \item \code{zclip} the provided value of \code{zclip}.

        \item \code{x0} and \code{x1}, numerical vectors of the sides of color
        intervals, and \code{col0} and \code{col1}, vectors of corresponding
        colors.  The meaning is the same as on input.  The purpose of returning
        these four vectors is to permit users to alter color mapping, as in
        example 3 in \dQuote{Examples}.
        
        \item \code{missingColor}, a color that could be used to specify
        missing values, e.g. as the same-named argument to
        \code{\link{imagep}}.  If this is supplied as an argument, its value is
        repeated in the return value.  Otherwise, its value is either
        \code{"gray"} or, in the case of \code{name} being given, the value in
        the GMT color map specification.

    }
}

\examples{
library(oce)

## Example 1. color scheme for points on xy plot
x <- seq(0, 1, length.out=40)
y <- sin(2 * pi * x)
par(mar=c(3, 3, 1, 1))
mar <- par('mar') # prevent margin creep by drawPalette()
## First, default breaks
c <- colormap(y)
drawPalette(c$zlim, col=c$col, breaks=c$breaks)
plot(x, y, bg=c$zcol, pch=21, cex=1)
grid()
par(mar=mar)
## Second, 100 breaks, yielding a smoother palette
c <- colormap(y, breaks=100)
drawPalette(c$zlim, col=c$col, breaks=c$breaks)
plot(x, y, bg=c$zcol, pch=21, cex=1)
grid()
par(mar=mar)

## Example 2. topographic image with a standard color scheme
par(mfrow=c(1,1))
data(topoWorld)
cm <- colormap(name="gmt_globe")
imagep(topoWorld, breaks=cm$breaks, col=cm$col)
## visualize color map
# plot(seq_along(cm$x0), cm$x0, pch=21, bg=cm$col0)
# grid()
# points(seq_along(cm$x1), cm$x1, pch=21, bg=cm$col1)

## Example 3. topographic image with modified colors
cm <- colormap(name="gmt_globe")
deep <- cm$x0 < -4000
cm$col0[deep] <- 'black'
cm$col1[deep] <- 'black'
cm <- colormap(x0=cm$x0, x1=cm$x1, col0=cm$col0, col1=cm$col1)
imagep(topoWorld, breaks=cm$breaks, col=cm$col)

## Example 4. image of world topography with water colorized 
##            smoothly from violet at 8km depth to blue
##            at 4km depth, then blending in 0.5km increments
##            to white at the coast, with tan for land.
cm <- colormap(x0=c(-8000, -4000, 0, 100),
               x1=c(-8000, -4000, 0, 100),
               col0=c("violet","blue","white","tan"),
               col1=c("violet","blue","white","tan"),
               blend=c(100, 8, 0))
lon <- topoWorld[['longitude']]
lat <- topoWorld[['latitude']]
z <- topoWorld[['z']]
imagep(lon, lat, z, breaks=cm$breaks, col=cm$col)
contour(lon, lat, z, levels=0, add=TRUE)
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    numerical specifications for some color maps are at
    \url{http://www.beamreach.org/maps/gmt/share/cpt},
    \url{http://soliton.vm.bytemark.co.uk/pub/cpt-city}, and other sources.}

\author{Dan Kelley}

\keyword{misc}


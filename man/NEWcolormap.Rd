\name{Colormap}

\alias{Colormap}

\title{Calculate color map (Draft Version)}

\description{Map values to colors, for use in palettes and plots.}

\usage{Colormap(z,
         breaks, col=oceColorsJet,
         name,
         x0, x1, col0, col1,
         n=1,
         missingColor="gray")}

\arguments{
  \item{z}{an optional vector or other set of numerical values to be examined.}

  \item{breaks}{an optional vector of numerical values for break points between
      color levels (see \code{\link{image}}).  If this is not provided, and if
      \code{colormap} is missing, then \code{pretty(z,n=10)} is used.  If
      \code{breaks} is provided but consists of only a single number, then
      again \code{\link{pretty}} is called, but with \code{n} set to that
      number.}
  
  \item{col}{either a vector of colors or a function taking a numerical value
      as its single argument and returning a vector of colors (see
      \code{\link{image}}.  The value of \code{col} is ignored if
      \code{name} is provided, or if \code{x0} through \code{col1}
      are provided.}

  \item{name}{an optional character string naming a built-in colormap (one of
      \code{"gmt_relief"}, \code{"gmt_ocean"}, \code{"gmt_globe"} or
      \code{"gmt_gebco"}) or a filename containing information describing the
      colormap in GMT format, e.g.
      \code{"http://www.beamreach.org/maps/gmt/share/cpt/GMT_globe.cpt"}). If
      \code{name} is provided, then all other arguments to the function are
      ignored.}

  \item{x0, x1, col0, col1}{Vectors that specify the colormap.  \code{x0}
      and \code{x1} are numerical values, while \code{col0} and \code{col1}
      are colors, either in the form of naming strings color such as
      \code{"red"} or in the form of the values returned by \code{\link{rgb}}
      or \code{\link{hsv}}.  }
  
  \item{n}{number of sub-intervals to use between successive values of \code{x0}, 
      etc.  This may be a single number, or a vector of numbers of length one
      less than the length of \code{x0}.}

  \item{missingColor}{colour to use for missing values.  This will be set
      automatically if \code{colormap} is given.}
} 

\details{(FIXME: add sentence or two on goal.)

    There are three ways of specifying color schemes:
    \enumerate{

        \item Supply \code{breaks}.  In this case, \code{breaks} and \code{col}
        are used together to specify a colour scheme.  If \code{col} is a
        function, then it is expected to take a single numerical argument that
        specifies the number of colors, and this number will be set to
        \code{length(breaks)-1}.  Otherwise, \code{col} may be a vector of
        colours, and its length must be one less than the number of breaks.
        (NB. if \code{breaks} is given, then all other arguments except
        \code{missingColor} are ignored.)

        \item Do not supply \code{breaks}, but supply \code{name} instead.
        This \code{name} may be the name of a pre-defined colour palette
        (\code{"gmt_relief"}, \code{"gmt_ocean"}, \code{"gmt_globe"} or
        \code{"gmt_gebco"}), or it may be the name of a file (including a URL)
        containing a color map in the GMT format (see \dQuote{References}).
        (NB. if \code{name} is given, then all other arguments except
        \code{missingColor} are ignored.)
        
        \item Do not supply either \code{breaks} or \code{name}, but instead
        supply each of \code{x0}, \code{x1}, \code{col0}, and \code{col1}.
        These values are specify a value-color mapping that is similar to that
        used for GMT color maps.  The method works by using \code{\link{seq}}
        to interpolate between the elements of the \code{x0} vector.  (FIXME
        state more clearly) The same is done for \code{x1}.  Similarly,
        \code{\link{colorRampPalette}} is used to interpolate between the
        colors in the \code{col0} vector, and the same is done for \code{col1}.
        (FIXME: explain more, perhaps by repeating a statement about how GMT is
        done.) In this way, a colormap can be constructed by specifying a few
        points.
    }
}
 

\value{FIXME: -- the below is rough --
    
    A list containing \code{zcol} (FIXME: explain this), \code{breaks},
    \code{col}, \code{x0}, \code{x1}, \code{col0}, \code{col1}, and
    \code{missingColor}. (FIXME: note that breaks and col etc are all new;
    these are not copies of the input.)}

\section{Development status}{Devised in early May 2014, this function is likely
    to be somewhat unstable until mid-summer 2014, with changes to the argument
    list and return value.
}
    
\examples{
## FIXME: these examples are all wrong, of course
\dontrun{
library(oce)
data(topoWorld)

## Topo map with GMT "globe" scheme
cm <- colormap("gmt_globe")
cz <- colorize(colormap=cm)
imagep(topoWorld, breaks=cz$breaks, col=cz$col)

## Modify to red for depths < 1km
cm <- colormap("gmt_globe")
km <- which(-1000 <= cm$x0 & cm$x0 < 0)
cm$col0[km] <- 'red'
cm$col1[km] <- 'red'
cz <- colorize(colormap=cm)
imagep(topoWorld, breaks=cz$breaks, col=cz$col)

## Construct color map
cm <- colormap(x0=c(-8000, -4000, 0, 100),
               x1=c(-8000, -4000, 0, 100),
               col0=c("violet","blue","white","tan"),
               col1=c("violet","blue","white","tan"),
               n=c(10, 10, 1))
cz <- colorize(colormap=cm)
imagep(topoWorld, breaks=cz$breaks, col=cz$col)
}
}

\references{Information on GMT software is given at
    \url{http://gmt.soest.hawaii.edu}.  Diagrams showing the GMT color schemes
    are at \url{http://www.geos.ed.ac.uk/it/howto/GMT/CPT/palettes.html}, and
    their numerical specifications are at e.g.
    \url{http://www.beamreach.org/maps/gmt/share/cpt}.}

\author{Dan Kelley}

\keyword{misc}


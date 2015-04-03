\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot coordinates as a map, using one of many possible projections
    calculated with the \code{mapproj} package or with \code{PROJ.4} [6] code
    included within oce. The projection parameters are stored in a local
    variable that can be retrieved by related functions, making it easy to add
    more items so the map, including points, lines, text, images and contours.}

\usage{mapPlot(longitude, latitude, longitudelim, latitudelim,
    grid=TRUE, bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE, showHemi=TRUE,
    polarCircle=0, lonlabel=NULL, latlabel=NULL, sides=NULL,
    projection="mollweide", parameters=NULL, orientation=NULL, trim=TRUE,
    debug=getOption("oceDebug"), \dots)
}

\arguments{

    \item{longitude}{longitudes of points to be plotted, or an object with a
        slot named \code{data} that contains items named \code{longitude} and
        \code{latitude} (e.g. of class \code{coastline}; see
        \code{\link{coastline-class}}), in which case position is inferred from
        the object and the second argument to this function is ignored.  If
        \code{longitude} is missing, it is set to
        \code{\link{coastlineWorld}}.}

    \item{latitude}{latitudes of points to be plotted}

    \item{longitudelim}{optional limit of longitudes to plot}

    \item{latitudelim}{optional limit of latitudes to plot}

    \item{grid}{either a number (or pair of numbers) indicating the spacing of
        longitude and latitude lines, in degrees, or a logical value (or pair
        of values) indicating whether to draw an auto-scaled grid, or whether
        to skip the grid drawing.  In the case of numerical values, \code{NA}
        can be used to turn off the grid in longitude or latitude.  For precise
        control, it can be best to set \code{grid=FALSE} and to add a grid
        later, using \code{\link{mapGrid}}.}

    \item{bg}{background colour for plot (ignored at present).}

    \item{fill}{colour to be used to fill land regions, or \code{NULL} to avoid
        filling.  Filling is inaccurate for projections, e.g. yielding as
        misshaped Antarticic continent or oceans filled instead of land. In
        such cases, it makes sense to turn filling off.}
        
    \item{type}{value to indicate type of plot, as with
        \code{\link{par}("plot")}.}

    \item{axes}{logical value indicating whether to draw longitude and latitude
        values in the lower and left margin, respectively.  This may not work
        well for some projections or scales.}

    \item{drawBox}{logical value indicating whether to draw a box around the
        plot.  This is helpful for many projections at sub-global scale.}

    \item{showHemi}{logical value indicating whether to show the hemisphere in
        axis tick labels.}

    \item{polarCircle}{a number indicating the number of degrees of latitude
        extending from the poles, within which zones are not drawn.}

    \item{lonlabel,latlabel,sides}{optional vectors of longitude and latitude
        to label on the indicated sides of plot, passed to
        \code{\link{plot.coastline}}.  Using these arguments permits reasonably
        simple customization.  If they are are not provided, reasonable
        defaults will be used.}

    \item{projection}{a string indicating the form of projection to be used.
        This may be in either of two formats.  The first is that used by
        \code{\link[mapproj]{mapproject}} in the \code{mapproj} package; if
        this is used, then the next two arguments are also used.  The second
        form, in which \code{projection} starts with ``\code{+proj=}'', is that
        used by \code{\link[proj4]{project}} in the \code{proj4} package.  Note
        that in this second case, there is no need to specify the scale
        parameter (\code{+k}), the easting parameter (\code{+easting}) or the
        northing parameter (\code{+northing}), since these are determined from
        the graph limits.  See \dQuote{Examples}.}

    \item{parameters, orientation}{parameters and orientation, if
        \code{projection} is in \code{mapproj} form; see
        \code{\link[mapproj]{mapproject}}.}

    \item{trim}{a logical value indicating whether to trim islands or lakes containing
        only points that are offscale of the current plot box.  This solves the problem
        of Antarctica overfilling the entire domain, for an Arctic-centred
        stereographic projection.  It is not a perfect solution, though,
        because the line segment joining two offscale points might intersect
        the plotting box.}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

    \item{\dots}{optional arguments passed to plotting functions.  This can be
        useful in many ways, e.g.  Example 5 shows how to use \code{xlim} etc
        to reproduce a scale exactly between two plots.}
}



\details{Creates a map using the indicated projection.  As noted in the
    information on the \code{projection} argument, projections may be
    specified in the format used by \code{\link[mapproj]{mapproject}} from
    the \code{mapproj} package, or the one used by
    \code{\link[proj4]{project}} from the \code{proj4} package.  The first
    of these packages is probably easier to learn but the second is more
    powerful and has a wider user base across multiple software systems
    beyond R.
    
    Once a projection is set, the functions that add to a map will follow
    the same projection.
    
    The available projections can be found in the documentation for
    \code{\link[mapproj]{mapproject}} and \code{\link[proj4]{project}}.

    For \code{PROJ.4}-style projections, the default is to use \code{PROJ.4}
    code that is built into Oce.  However, if
    \code{options(externalProj4=TRUE)}, then the \code{proj4} package will be
    used instead.  Generally, the default method is better because the
    \code{proj4} package handles unprojectable points poorly, so that
    coordinate vectors containing such points must be processed one point at a
    time, at considerable penalty in speed.  For example, using the external
    \code{proj4} package on a worldwide mercator plot yields a slowdown by a
    factor of 50X.

    Further details on map projections are provided by [1], an exhaustive
    treatment that includes many illustrations, an overview of the history of
    the topic, and some notes on the strengths and weaknesses of the various
    formulations.  See especially pages 2 through 7, which define terms and
    provide recommendations.  References [2] and [3] are also useful,
    especially regarding datum shifts; [4] and [5] are less detailed and
    perhaps better for novices.
    
}

\seealso{Points may be added to a map with \code{\link{mapPoints}}, lines
    with \code{\link{mapLines}}, text with \code{\link{mapText}}, polygons
    with \code{\link{mapPolygon}}, images with \code{\link{mapImage}}, and
    scale bars with \code{\link{mapScalebar}}..  Points on a map may be
    determined with mouse clicks using \code{\link{mapLocator}}.  Great
    circle paths can be calculated with \code{\link{geodGc}}.}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)

# Example 1.
# Mollweide ([1] page 54) is an equal-area projection that works well
# for whole-globe views, below shown in a Pacific-focus view using
# both the mapproj and PROJ.4 formats.  (This pattern of showing the two
# formats is repeated in all the examples.)
par(mfrow=c(1,2), mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, proj="mollweide", orientation=c(90,-180,0))
mtext("Mollweide/mapproj", adj=1)
mapPlot(coastlineWorld, proj="+proj=moll +lon_0=-180")
mtext("Mollweide/proj4", adj=1)

# Example 2.
# Orthographic projections resemble a globe, making them attractive for
# non-technical use, but they are neither conformal nor equal-area, so they
# are somewhat limited for serious use on large scales.  See Section 20 of
# [1].
par(mfrow=c(1,2), mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, projection="orthographic",
        orientation=c(0,-180,0))
mtext("Orthographic/mapproj", adj=1)
mapPlot(coastlineWorld, projection="+proj=ortho +lon_0=-180")
mtext("Orthographic/proj4", adj=1)

# Example 3.
# The Lambert conformal conic projection is an equal-area projection
# recommended by [1], page 95, for regions of large east-west extent
# away from the equator, here illustrated for the USA and Canada.
par(mfrow=c(2,1), mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
        proj="lambert", parameters=c(lat0=40,lat1=60),
        orientation=c(90, -100, 0))
mtext("Lambert/mapproj", adj=1)
mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
        projection="+proj=lcc +lat_0=50 +lat_1=60 +lon_0=-100")
mtext("Lambert/proj4", adj=1)

# Example 4.
# The stereographic projection [1], page 120, is conformal, used
# below for an Arctic view with a Canadian focus.  Note the trick of going
# past the pole: the second latitudelim value is 180 minus the first, and the
# second longitudelim is 180 plus the first; this uses image points "over"
# the pole.
par(mfrow=c(1,2), mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, longitudelim=c(-130,50), latitudelim=c(70,110),
        proj="stereographic", orientation=c(90, -135, 0), fill='gray') 
mtext("Stereographic/mapproj", adj=1)
mapPlot(coastlineWorld, longitudelim=c(-130,50), latitudelim=c(70,110),
        proj="+proj=stere +lat_0=90 +lon_0=-135") #, fill='gray')
mtext("Stereographic/proj4", adj=1)

# Example 5.
# Spinning globe: create PNG files that can be assembled into a movie
png("539B-\%03d.png")
lons <- seq(360, 0, -15)
ilon <- seq_along(lons)
par(mar=rep(0, 4))
for (i in ilon) {
    p <- paste("+proj=ortho +lat_0=30 +lon_0=", lons[i], sep="")
    if (i == 1) {
        mapPlot(coastlineWorld, projection=p, col="blue", lwd=1.4)
        xlim <- par("usr")[1:2]
        ylim <- par("usr")[3:4]
    } else {
        mapPlot(coastlineWorld, projection=p, col="blue", lwd=1.4,
                xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
    }
}
dev.off()
}
}

\section{Available projections}{All of the projections provided in the
    \code{mapproj} package are available, but the lack of inverses in that
    package mean that sometimes the plot has flaws (especially on axis
    labelling). The recommended method is to use the \code{PROJ.4} [6] scheme,
    which permits the following projections [7]:
    \code{"aea"}, \code{"aeqd"}, \code{"aitoff"}, \code{"alsk"}, \code{"bipc"},
    \code{"bonne"}, \code{"cass"}, \code{"cc"}, \code{"cea"}, \code{"collg"},
    \code{"crast"}, \code{"eck1"}, \code{"eck2"}, \code{"eck3"}, \code{"eck4"},
    \code{"eck5"}, \code{"eck6"}, \code{"eqc"}, \code{"eqdc"}, \code{"euler"},
    \code{"etmerc"}, \code{"fahey"}, \code{"fouc"}, \code{"fouc_s"},
    \code{"gall"}, \code{"geos"}, \code{"gn_sinu"}, \code{"gnom"},
    \code{"goode"}, \code{"gs48"}, \code{"gs50"}, \code{"hatano"},
    \code{"healpix"}, \code{"rhealpix"}, \code{"igh"}, \code{"imw_p"},
    \code{"isea"}, \code{"kav5"}, \code{"kav7"}, \code{"krovak"},
    \code{"labrd"}, \code{"laea"}, \code{"lonlat"}, \code{"latlon"},
    \code{"lcc"}, \code{"lcca"}, \code{"leac"}, \code{"lee_os"},
    \code{"loxim"}, \code{"lsat"}, \code{"mbt_s"}, \code{"mbt_fps"},
    \code{"mbtfpp"}, \code{"mbtfpq"}, \code{"mbtfps"}, \code{"merc"},
    \code{"mil_os"}, \code{"mill"}, \code{"moll"}, \code{"murd1"},
    \code{"murd2"}, \code{"murd3"}, \code{"natearth"}, \code{"nell"},
    \code{"nell_h"}, \code{"nsper"}, \code{"nzmg"}, \code{"ob_tran"},
    \code{"ocea"}, \code{"oea"}, \code{"omerc"}, \code{"ortho"},
    \code{"pconic"}, \code{"poly"}, \code{"putp1"}, \code{"putp2"},
    \code{"putp3"}, \code{"putp3p"}, \code{"putp4"}, \code{"putp4p"},
    \code{"putp5"}, \code{"putp5p"}, \code{"putp6"}, \code{"putp6p"},
    \code{"qsc"}, \code{"qua_aut"}, \code{"robin"}, \code{"rouss"},
    \code{"sinu"}, \code{"somerc"}, \code{"stere"}, \code{"sterea"},
    \code{"gstmerc"}, \code{"tcea"}, \code{"tissot"}, \code{"tmerc"},
    \code{"tpeqd"}, \code{"tpers"}, \code{"ups"}, \code{"urm5"},
    \code{"urmfps"}, \code{"utm"}, \code{"vandg"}, \code{"vitk1"},
    \code{"wag1"}, \code{"wag2"}, \code{"wag3"}, \code{"wag4"}, \code{"wag5"},
    \code{"wag6"}, \code{"weren"}, and \code{"wink1"}. (See [7] for invertible
    projections that are omitted in oce.)
    The PROJ.4 documentation [6] details the additional parameters that may be
    used with each projection.
}

\section{Choosing a projection}{The use of the PROJ.4 scheme is greatly
    encouraged. The best choice of projection depends on the application.
    Readers may find \code{projection="+proj=moll"} useful for world-wide
    plots, \code{ortho} for hemispheres viewed from the equator, \code{stere}
    for polar views, \code{lcc} for wide meridional ranges in mid latitudes,
    and \code{merc} in limited-area cases where angle preservation is
    important.}

\section{Issues}{Map projection is a complicated matter that is addressed here
    in a limited and pragmatic way.  For example, \code{mapPlot} tries to draw
    axes along a box containing the map, instead of trying to find spots along
    the ``edge'' of the map at which to put longitude and latitude labels.
    This design choice greatly simplifies the coding effort, freeing up time to
    work on issues regarded as more pressing.  Chief among those issues are (a)
    the occurrence of horizontal lines in maps that have employ prime meridians
    (b) inaccurate filling of land regions that (again) occur with shifted
    meridians and (c) inaccurate filling of Antarctica in some projections.
    Generally, issues are tackled first for commonly used projections, such as
    those used in the examples.}

\author{Dan Kelley and Clark Richards}

\references{1. Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
    Professional Paper: 1395 (available at
    \url{pubs.usgs.gov/pp/1395/report.pdf}).

    2. \url{http://earth-info.nga.mil/GandG/publications/tm8358.1/toc.html}

    3. Natural Resources Canada \url{http://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/9805}

    4. Wikipedia page \url{http://en.wikipedia.org/wiki/List_of_map_projections}

    5. Radical Cartography website \url{http://www.radicalcartography.net/?projectionref}

    6. \code{PROJ.4} website \url{http://trac.osgeo.org/proj}

    7. All the \code{PROJ.4} (version 4.9.0) schemes that have inverses are
    used, except for \code{calcofi} (which is not really a projection scheme),
    \code{isea} (which causes R to segmentation-fault on a world coastline test
    case), \code{labrd} (which is a niche local projection that has an error
    making it return \code{NaN} for the entire earth coastline) and
    \code{wintri} (which hangs R when plotting the world coastline).
}


\keyword{hplot}


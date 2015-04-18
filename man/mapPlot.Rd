\name{mapPlot}

\alias{mapPlot}

\title{Plot a map}

\description{Plot coordinates as a map, using one of many possible projections
    calculated with the \code{PROJ.4} [6] code that is included within oce.
    (For a temporary alternative, see \dQuote{Deprecation notice}.) The
    projection parameters are stored in a local variable that can be retrieved
    by related functions, making it easy to add more items so the map,
    including points, lines, text, images and contours.}

\usage{mapPlot(longitude, latitude, longitudelim, latitudelim,
    grid=TRUE, bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE, showHemi=TRUE,
    polarCircle=0, lonlabel=NULL, latlabel=NULL, sides=NULL,
    projection="+proj=moll", parameters=NULL, orientation=NULL,
    tissot=FALSE, trim=TRUE, debug=getOption("oceDebug"), \dots) }

\arguments{

    \item{longitude}{longitudes of points to be plotted, or an object with a
        slot named \code{data} that contains items named \code{longitude} and
        \code{latitude} (e.g. an \code{\link{coastline-class}} object), in
        which case position is inferred from the object and the second argument
        to this function is ignored.  If \code{longitude} is missing, it is set
        to \code{\link{coastlineWorld}}, which is a moderate-resolution
        coastline object.}

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

    \item{projection}{optional indication of projection.  This is normally a
        string in the format used by the \code{proj4} package, i.e.  starting
        with \code{+proj=}. (See \dQuote{Deprecation notice}.)}

    \item{parameters,orientation}{See \dQuote{Deprecation notice}.}

    \item{trim}{a logical value indicating whether to trim islands or lakes
        containing only points that are off-scale of the current plot box.
        This solves the problem of Antarctica overfilling the entire domain,
        for an Arctic-centred stereographic projection.  It is not a perfect
        solution, though, because the line segment joining two off-scale points
        might intersect the plotting box.}
    
    \item{tissot}{logical indicating whether to use \code{\link{mapTissot}} to
        plot Tissot indicatrices, i.e. ellipses at grid intersection points,
        which indicate map distortion.}

    \item{debug}{a flag that turns on debugging.  Set to 1 to get a moderate
        amount of debugging information, or to 2 to get more.} 

    \item{\dots}{optional arguments passed to some plotting functions.  This can be
        useful in many ways, e.g.  Example 5 shows how to use \code{xlim} etc
        to reproduce a scale exactly between two plots.}
}

\details{Creates a map using the indicated projection.  As noted in the
    information on the \code{projection} argument, projections are best
    specified in the notation used by \code{project()} in the \code{proj4}
    package, although (at least temporarily) it is also possible to specify it
    in the notation of \code{mapproject()} in the \code{mapproj} package.
    
    Once a projection is set, the many functions that add more material to a
    map will use that same projection.
    
    %For \code{PROJ.4}-style projections, the default is to use \code{PROJ.4}
    %code that is built into Oce.  However, in a feature that may be eliminated
    %at any time without notice, if \code{options(externalProj4=TRUE)}, then the
    %\code{proj4} package will be used instead.  The default method is much
    %better because the \code{proj4} package handles unprojectable points
    %poorly, so that coordinate vectors containing such points must be processed
    %one point at a time, at considerable penalty in speed.  For example, using
    %the external \code{proj4} package on a worldwide Mercator plot yields a
    %slowdown by a factor of 50X.

    Further details on map projections are provided by [1,11], an exhaustive
    treatment that includes many illustrations, an overview of the history of
    the topic, and some notes on the strengths and weaknesses of the various
    formulations.  See especially pages 2 through 7, which define terms and
    provide recommendations.  Reference [2] is also useful,
    especially regarding datum shifts; [3] and [4] are less detailed and
    perhaps better for novices. See [8] for a gallery of projections.
    
}

\section{Deprecation notice}{At the moment of writing, it is still also
    possible to specify projections using \code{projection}, \code{parameters}
    and \code{orientation} in the terminology employed by the \code{mapproj}
    package. In that case, the calculations are done using that package (if it
    is installed). Unfortunately, this method can yield poor results, because
    inverse-projection is not handled by \code{mapproj}. For this reason, this
    capability is likely to be removed in a future version of \code{oce}. It is
    only provided in the present version to give users time to alter their
    code. It will be removed sometime in the year 2015.}



\seealso{Points may be added to a map with \code{\link{mapPoints}}, lines
    with \code{\link{mapLines}}, text with \code{\link{mapText}}, polygons with
    \code{\link{mapPolygon}}, images with \code{\link{mapImage}}, and scale
    bars with \code{\link{mapScalebar}}.  Points on a map may be determined
    with mouse clicks using \code{\link{mapLocator}}.  Great circle paths can
    be calculated with \code{\link{geodGc}}.  See [8] for a demonstration of
    the available map projections (with graphs).}

\examples{
\dontrun{
library(oce)
data(coastlineWorld)

# Example 1.
# Mollweide ([1] page 54) is an equal-area projection that works well
# for whole-globe views, below shown in a Pacific-focus view.
# Note that filling is not employed when the prime meridian
# is shifted, because this causes a problem with Antarctica
par(mfrow=c(2,1), mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, projection="+proj=moll", fill='gray')
mtext("Mollweide", adj=1)
mapPlot(coastlineWorld, projection="+proj=moll +lon_0=-180")
mtext("Mollweide", adj=1)
par(mfrow=c(1,1))

# Example 2.
# Orthographic projections resemble a globe, making them attractive for
# non-technical use, but they are neither conformal nor equal-area, so they
# are somewhat limited for serious use on large scales.  See Section 20 of
# [1]. Note that filling is not employed because it causes a problem with 
# Antarctica.
par(mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, projection="+proj=ortho +lon_0=-180")
mtext("Orthographic", adj=1)

# Example 3.
# The Lambert conformal conic projection is an equal-area projection
# recommended by [1], page 95, for regions of large east-west extent
# away from the equator, here illustrated for the USA and Canada.
par(mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, longitudelim=c(-130,-55), latitudelim=c(35,60),
        projection="+proj=lcc +lat_0=30 +lat_1=60 +lon_0=-100", fill='gray')
mtext("Lambert conformal", adj=1)

# Example 4.
# The stereographic projection [1], page 120, is conformal, used
# below for an Arctic view with a Canadian focus.  Note the trick of going
# past the pole: the second latitudelim value is 180 minus the first, and the
# second longitudelim is 180 plus the first; this uses image points "over"
# the pole.
par(mar=c(3, 3, 1, 1))
mapPlot(coastlineWorld, longitudelim=c(-130,50), latitudelim=c(70,110),
        proj="+proj=stere +lat_0=90 +lon_0=-135", fill='gray')
mtext("Stereographic", adj=1)

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

%\section{Available projections}{All of the projections provided in the
%    \code{mapproj} package are available, but the lack of inverses in that
%    package mean that sometimes the plot has flaws (especially on axis
%    labelling). The recommended method is to use the \code{PROJ.4} [6] scheme,
%    which permits the following projections [7]:
%    \code{"aea"}, \code{"aeqd"}, \code{"aitoff"}, \code{"alsk"}, \code{"bipc"},
%    \code{"bonne"}, \code{"cass"}, \code{"cc"}, \code{"cea"}, \code{"collg"},
%    \code{"crast"}, \code{"eck1"}, \code{"eck2"}, \code{"eck3"}, \code{"eck4"},
%    \code{"eck5"}, \code{"eck6"}, \code{"eqc"}, \code{"eqdc"}, \code{"euler"},
%    \code{"etmerc"}, \code{"fahey"}, \code{"fouc"}, \code{"fouc_s"},
%    \code{"gall"}, \code{"geos"}, \code{"gn_sinu"}, \code{"gnom"},
%    \code{"goode"}, \code{"gs48"}, \code{"gs50"}, \code{"hatano"},
%    \code{"healpix"}, \code{"rhealpix"}, \code{"igh"}, \code{"imw_p"},
%    \code{"isea"}, \code{"kav5"}, \code{"kav7"}, \code{"krovak"},
%    \code{"labrd"}, \code{"laea"}, \code{"lonlat"}, \code{"latlon"},
%    \code{"lcc"}, \code{"lcca"}, \code{"leac"}, \code{"lee_os"},
%    \code{"loxim"}, \code{"lsat"}, \code{"mbt_s"}, \code{"mbt_fps"},
%    \code{"mbtfpp"}, \code{"mbtfpq"}, \code{"mbtfps"}, \code{"merc"},
%    \code{"mil_os"}, \code{"mill"}, \code{"moll"}, \code{"murd1"},
%    \code{"murd2"}, \code{"murd3"}, \code{"natearth"}, \code{"nell"},
%    \code{"nell_h"}, \code{"nsper"}, \code{"nzmg"}, \code{"ob_tran"},
%    \code{"ocea"}, \code{"oea"}, \code{"omerc"}, \code{"ortho"},
%    \code{"pconic"}, \code{"poly"}, \code{"putp1"}, \code{"putp2"},
%    \code{"putp3"}, \code{"putp3p"}, \code{"putp4"}, \code{"putp4p"},
%    \code{"putp5"}, \code{"putp5p"}, \code{"putp6"}, \code{"putp6p"},
%    \code{"qsc"}, \code{"qua_aut"}, \code{"robin"}, \code{"rouss"},
%    \code{"sinu"}, \code{"somerc"}, \code{"stere"}, \code{"sterea"},
%    \code{"gstmerc"}, \code{"tcea"}, \code{"tissot"}, \code{"tmerc"},
%    \code{"tpeqd"}, \code{"tpers"}, \code{"ups"}, \code{"urm5"},
%    \code{"urmfps"}, \code{"utm"}, \code{"vandg"}, \code{"vitk1"},
%    \code{"wag1"}, \code{"wag2"}, \code{"wag3"}, \code{"wag4"}, \code{"wag5"},
%    \code{"wag6"}, \code{"weren"}, and \code{"wink1"}. (See [7] for invertible
%    projections that are omitted in oce.)
%    The PROJ.4 documentation [6] details the additional parameters that may be
%    used with each projection.
%}

\section{Available Projections}{Users who choose the (unrecommended) mapproj
    scheme for setting the projection should consult the documentation for
    \code{mapproject} in the \code{mapproj} package. Those who choose the
    (recommended) PROJ.4 system [5,6] have a wide range of projections to
    choose from. These are listed in the table given below.  For example, set
    \code{projection="+proj=aea"} to select the Albers equal area projection.

    The projection list consists of all the PROJ.4 (version 4.9.1) projections
    that have inverses, minus a few that cause problems:
    \code{alsk} overdraws \code{coastlineWorld}, and is a niche projection for Alaska;
    \code{calcofi} is not a real projection, but rather a coordinate system;
    \code{gs48} overdraws \code{coastlineWorld}, and is a niche projection for the USA;
    \code{gs50} overdraws \code{coastlineWorld}, and is a niche projection for the USA;
    \code{gstmerc} overdraws \code{coastlineWorld};
    \code{isea} causes segmentation faults on OSX systems; 
    \code{krovak} overdraws \code{coastlineWorld}, and is a niche projection for the Czech Republic;
    \code{labrd} returns \code{NaN} for most of the world, and is a niche projection for Madagascar;
    \code{lee_os} overdraws \code{coastlineWorld};
    and
    \code{nzmg} overdraws \code{coastlineWorld}.
    
    
    \code{labrd} returns \code{NaN} for
    most points in \code{coastlineWorld} (and seems to be designed only for
    Madagascar);

    The information in the table is reformatted from the output of the unix
    command \code{proj -lP}, where \code{proj} is provided by version 4.9.0 of
    the PROJ.4 system. Most of the arguments listed have default values. In
    addition, most projections can handle arguments \code{lon_0} and
    \code{lat_0}, for shifting the reference point, although in some cases
    shifting the longitude can yield poor filling of coastlines.

    Further details of the projections and the controlling arguments are
    provided at several websites, because PROJ.4 has been incorporated into
    many software systems; a good starting point for learning is [6].  
    
    See \dQuote{Examples} for suggested projections for some common
    applications, and [8] for a gallery indicating how to use every projection.

\tabular{lll}{
\strong{Projection}                       \tab \strong{Code}   \tab \strong{Arguments}\cr
Albers equal area                         \tab \code{aea}      \tab \code{lat_1}, \code{lat_2}\cr
Azimuthal equidistant                     \tab \code{aeqd}     \tab \code{lat_0}, \code{guam}\cr
Aitoff                                    \tab \code{aitoff}   \tab - \cr
Mod. stererographics of Alaska            \tab \code{alsk}     \tab - \cr
Bipolar conic of western hemisphere       \tab \code{bipc}     \tab - \cr
Bonne Werner                              \tab \code{bonne}    \tab \code{lat_1}\cr
Cassini                                   \tab \code{cass}     \tab - \cr
Central cylindrical                       \tab \code{cc}       \tab - \cr
Equal area cylindrical                    \tab \code{cea}      \tab \code{lat_ts}\cr
Collignon                                 \tab \code{collg}    \tab - \cr
Craster parabolic Putnins P4              \tab \code{crast}    \tab - \cr
Eckert I                                  \tab \code{eck1}     \tab - \cr
Eckert II                                 \tab \code{eck2}     \tab - \cr
Eckert III                                \tab \code{eck3}     \tab - \cr
Eckert IV                                 \tab \code{eck4}     \tab - \cr
Eckert V                                  \tab \code{eck5}     \tab - \cr
Eckert VI                                 \tab \code{eck6}     \tab - \cr
Equidistant cylindrical plate (Caree)     \tab \code{eqc}      \tab \code{lat_ts}, \code{lat_0}\cr
Equidistant conic                         \tab \code{eqdc}     \tab \code{lat_1}, \code{lat_2}\cr
Euler                                     \tab \code{euler}    \tab \code{lat_1}, \code{lat_2}\cr
Extended transverse Mercator              \tab \code{etmerc}   \tab \code{lat_ts}, \code{lat_0}\cr
Fahey                                     \tab \code{fahey}    \tab - \cr
Foucaut                                   \tab \code{fouc}     \tab - \cr
Foucaut sinusoidal                        \tab \code{fouc_s}   \tab - \cr
Gall stereographic                        \tab \code{gall}     \tab - \cr
Geostationary satellite view              \tab \code{geos}     \tab \code{h}\cr
General sinusoidal series                 \tab \code{gn_sinu}  \tab \code{m}, \code{n}\cr
Gnomonic                                  \tab \code{gnom}     \tab - \cr
Goode homolosine                          \tab \code{goode}    \tab - \cr
%Mod. stererographics of 48 U.S.           \tab \code{gs48}     \tab - \cr
%Mod. stererographics of 50 U.S.           \tab \code{gs50}     \tab - \cr
Hatano asymmetrical equal area            \tab \code{hatano}   \tab - \cr
HEALPix                                   \tab \code{healpix}  \tab - \cr
rHEALPix                                  \tab \code{rhealpix} \tab \code{north_square}, \code{south_square}\cr
Interrupted Goode homolosine              \tab \code{igh}      \tab -\cr
International map of the world polyconic  \tab \code{imw_p}    \tab \code{lat_1}, \code{lat_2}, \code{lon_1}\cr
Kavraisky V                               \tab \code{kav5}     \tab - \cr
Kavraisky VII                             \tab \code{kav7}     \tab - \cr
%Krovak                                    \tab \code{krovak}   \tab - \cr
Lambert azimuthal equal area              \tab \code{laea}     \tab - \cr
Lat/long                                  \tab \code{lonlat}   \tab - \cr
Lat/long                                  \tab \code{latlon}   \tab - \cr
Lambert conformal conic                   \tab \code{lcc}      \tab \code{lat_1}, \code{lat_2}, \code{lat_0}\cr
Lambert conformal conic alternative       \tab \code{lcca}     \tab \code{lat_0}\cr
Lambert equal area conic                  \tab \code{leac}     \tab \code{lat_1}, \code{south}\cr
Lee oblated stereographic                 \tab \code{lee_os}   \tab\cr
Loximuthal                                \tab \code{loxim}    \tab\cr
Space oblique for Landsat                 \tab \code{lsat}     \tab \code{lsat}, \code{path}\cr
McBryde-Thomas flat-polar sine, no. 1     \tab \code{mbt_s}    \tab\cr
McBryde-Thomas flat-polar sine, no. 2     \tab \code{mbt_fps}  \tab\cr
McBride-Thomas flat-polar parabolic       \tab \code{mbtfpp}   \tab\cr
McBryde-Thomas flat-polar quartic         \tab \code{mbtfpq}   \tab\cr
McBryde-Thomas flat-polar sinusoidal      \tab \code{mbtfps}   \tab\cr
Mercator                                  \tab \code{merc}     \tab \code{lat_ts}\cr
Miller oblated stereographic              \tab \code{mil_os}   \tab\cr
Miller cylindrical                        \tab \code{mill}     \tab\cr
Mollweide                                 \tab \code{moll}     \tab\cr
Murdoch I                                 \tab \code{murd1}    \tab \code{lat_1}, \code{lat_2}\cr
Murdoch II                                \tab \code{murd2}    \tab \code{lat_1}, \code{lat_2}\cr
murdoch III                               \tab \code{murd3}    \tab \code{lat_1}, \code{lat_2}\cr
Natural earth                             \tab \code{natearth} \tab\cr
Nell                                      \tab \code{nell}     \tab\cr
Nell-Hammer                               \tab \code{nell_h}   \tab\cr
Near-sided perspective                    \tab \code{nsper}    \tab \code{h}\cr
New Zealand map grid                      \tab \code{nzmg}     \tab\cr
General oblique transformation            \tab \code{ob_tran}  \tab \code{o_proj}, \code{o_lat_p}, \code{o_lon_p}, \code{o_alpha}, \code{o_lon_c}\cr
                                          \tab                 \tab \code{o_lat_c}, \code{o_lon_1}, \code{o_lat_1}, \code{o_lon_2}, \code{o_lat_2}\cr
Oblique cylindrical equal area            \tab \code{ocea}     \tab \code{lat_1}, \code{lat_2}, \code{lon_1}, \code{lon_2}\cr
Oblated equal area                        \tab \code{oea}      \tab \code{n}, \code{m}, \code{theta}\cr
Oblique Mercator                          \tab \code{omerc}    \tab \code{alpha}, \code{gamma}, \code{no_off}, \code{lonc}, \code{lon_1},\cr
                                          \tab                 \tab \code{lat_1}, \code{lon_2}, \code{lat_2}\cr
Orthographic                              \tab \code{ortho}    \tab - \cr
Perspective conic                         \tab \code{pconic}   \tab \code{lat_1}, \code{lat_2}\cr
Polyconic American                        \tab \code{poly}     \tab - \cr
Putnins P1                                \tab \code{putp1}    \tab - \cr
Putnins P2                                \tab \code{putp2}    \tab - \cr
Putnins P3                                \tab \code{putp3}    \tab - \cr
Putnins P3'                               \tab \code{putp3p}   \tab - \cr
Putnins P4'                               \tab \code{putp4p}   \tab - \cr
Putnins P5                                \tab \code{putp5}    \tab - \cr
Putnins P5'                               \tab \code{putp5p}   \tab - \cr
Putnins P6                                \tab \code{putp6}    \tab - \cr
Putnins P6'                               \tab \code{putp6p}   \tab - \cr
Quartic authalic                          \tab \code{qua_aut}  \tab - \cr
Quadrilateralized spherical cube          \tab \code{qsc}      \tab - \cr
Robinson                                  \tab \code{robin}    \tab - \cr
Roussilhe stereographic                   \tab \code{rouss}    \tab - \cr
Sinusoidal aka Sanson-Flamsteed           \tab \code{sinu}     \tab - \cr
Swiss. oblique Mercator                   \tab \code{somerc}   \tab - \cr
Stereographic                             \tab \code{stere}    \tab \code{lat_ts}\cr
Oblique stereographic alternative         \tab \code{sterea}   \tab - \cr
Gauss-schreiber transverse Mercator       \tab \code{gstmerc}  \tab \code{lat_0}, \code{lon_0}, \code{k_0}\cr
Transverse cylindrical equal area         \tab \code{tcea}     \tab - \cr
Tissot                                    \tab \code{tissot}   \tab \code{lat_1}, \code{lat_2}\cr
Transverse Mercator                       \tab \code{tmerc}    \tab - \cr
Two point equidistant                     \tab \code{tpeqd}    \tab \code{lat_1}, \code{lon_1}, \code{lat_2}, \code{lon_2}\cr
Tilted perspective                        \tab \code{tpers}    \tab \code{tilt}, \code{azi}, \code{h}\cr
Universal polar stereographic             \tab \code{ups}      \tab \code{south}\cr
Urmaev flat-polar sinusoidal              \tab \code{urmfps}   \tab \code{n}\cr
Universal transverse Mercator             \tab \code{utm}      \tab \code{zone}, \code{south}\cr
van der Grinten I                         \tab \code{vandg}    \tab - \cr
Vitkovsky I                               \tab \code{vitk1}    \tab \code{lat_1}, \code{lat_2}\cr
Wagner I Kavraisky VI                     \tab \code{wag1}     \tab - \cr
Wagner II                                 \tab \code{wag2}     \tab - \cr
Wagner III                                \tab \code{wag3}     \tab \code{lat_ts}\cr
Wagner IV                                 \tab \code{wag4}     \tab - \cr
Wagner V                                  \tab \code{wag5}     \tab - \cr
Wagner VI                                 \tab \code{wag6}     \tab - \cr
Werenskiold I                             \tab \code{weren}    \tab - \cr
Winkel I                                  \tab \code{wink1}    \tab \code{lat_ts}\cr
Winkel Tripel                             \tab \code{wintri}   \tab \code{lat_ts}\cr
}
}

\section{Available ellipse formulations}{In the PROJ.4 system of specifying
    projections, the following ellipse models are available: \code{MERIT},
    \code{SGS85}, \code{GRS80}, \code{IAU76}, \code{airy}, \code{APL4.9},
    \code{NWL9D}, \code{mod_airy}, \code{andrae}, \code{aust_SA}, \code{GRS67},
    \code{bessel}, \code{bess_nam}, \code{clrk66}, \code{clrk80},
    \code{clrk80ign}, \code{CPM}, \code{delmbr}, \code{engelis},
    \code{evrst30}, \code{evrst48}, \code{evrst56}, \code{evrst69},
    \code{evrstSS}, \code{fschr60}, \code{fschr60m}, \code{fschr68},
    \code{helmert}, \code{hough}, \code{intl}, \code{krass}, \code{kaula},
    \code{lerch}, \code{mprts}, \code{new_intl}, \code{plessis}, \code{SEasia},
    \code{walbeck}, \code{WGS60}, \code{WGS66}, \code{WGS72}, \code{WGS84}, and
    \code{sphere} (the default).  For example, use \code{projection="+proj=aea
        +ellps=WGS84"} for an Albers Equal Area projection using the most
    recent of the World Geodetic System model. It is unlikely that changing the
    ellipse will have a visible effect on plotted material at the plot scale
    appropriate to most oceanographic applications.}

\section{Available datum formulations}{In the PROJ.4 system of specifying
    projections, the following datum formulations are available: \code{WGS84},
    \code{GGRS87}, \code{Greek_Geodetic_Reference_System_1987}, \code{NAD83},
    \code{North_American_Datum_1983}, \code{NAD27},
    \code{North_American_Datum_1927}, \code{potsdam}, \code{Potsdam},
    \code{carthage}, \code{Carthage}, \code{hermannskogel},
    \code{Hermannskogel}, \code{ire65}, \code{Ireland}, \code{nzgd49},
    \code{New}, \code{OSGB36}, and \code{Airy}. It is unlikely that changing
    the datum will have a visible effect on plotted material at the plot scale
    appropriate to most oceanographic applications.}

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

    2. Natural Resources Canada \url{http://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/9805}

    3. Wikipedia page \url{http://en.wikipedia.org/wiki/List_of_map_projections}

    4. Radical Cartography website \url{http://www.radicalcartography.net/?projectionref}

    5. \code{PROJ.4} website \url{http://trac.osgeo.org/proj}

    6. \code{PROJ.4} projection details are at \url{http://www.remotesensing.org/geotiff/proj_list/}.

    7. All the \code{PROJ.4} (version 4.9.0) schemes that have inverses are
    used, except for \code{calcofi} (which is not really a projection scheme),
    \code{isea} (which causes R to segmentation-fault on a world coastline test
    case), and \code{labrd} which a nich local projection that has an error
    making it return \code{NaN} for the entire earth coastline.

    8. A gallery of plots with the PROJ.4-style projections is provided at
    \url{http://dankelley.github.io/r/2015/04/03/oce-proj.html}.  

    9. A website with illustrated explanations of projections, including
    historical notes, is
    \url{http://www.progonos.com/furuti/MapProj/CartIndex/cartIndex.html}.

    10. A fascinating historical perspective is provided by Snyder, J. P.
    (1993). Two thousand years of map projections. University of Chicago Press.

}

\keyword{hplot}


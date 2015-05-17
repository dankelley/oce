\name{imagep}

\alias{imagep}

\title{Plot an image with a color palette}

\description{Plot an image with a color palette}

\usage{imagep(x, y, z, 
    xlim, ylim, zlim, 
    zclip=FALSE, flipy=FALSE,
    xlab="", ylab="", zlab="", zlabPosition=c("top", "side"),
    decimate=TRUE,
    breaks, col, colormap, labels=NULL, at=NULL,
    drawContours=FALSE,
    drawPalette=TRUE,
    drawTriangles=FALSE,
    tformat,
    drawTimeRange=getOption("oceDrawTimeRange"),
    filledContour=FALSE,
    missingColor=NULL,
    mgp=getOption("oceMgp"),
    mar, mai.palette,
    xaxs="i", yaxs="i",
    cex=par("cex"),
    adorn,
    axes=TRUE,
    main="",
    axisPalette,
    debug=getOption("oceDebug"),
    \dots)
}

\arguments{

    \item{x,y}{optional locations of grid lines along which values of \code{z}
        are measured.   The values must be finite and distinct.  If they are
        out of order, they will be ordered (and the \code{z} data will be
        ordered in a matching way).  If \code{x} and \code{y} are not provided,
        they are constructed to indicate the indices of the matrix, in contrast
        to the range of 0 to 1, as is the case for \code{\link{image}}.  If
        \code{x} is a list, its components \code{x$x} and \code{x$y} are used
        for \code{x} and \code{y}, respectively. If the list has component
        \code{z} this is used for \code{z}. (NOTE: these arguments are meant to
        mimic those of \code{\link{image}}, which explains the same description
        here.)  There are also some special cases, e.g. if \code{x} is a
        topographic object such as can be created with \code{\link{read.topo}}
        or \code{\link{as.topo}}, then longitude and latitude are used for
        axes, and topographic height is drawn.}

    \item{z}{a matrix containing the values to be plotted (NAs are allowed). Note
        that x can be used instead of z for convenience. (NOTE: these arguments
        are meant to mimic those of \code{\link{image}}, which explains the same
        description here.)} 
    \item{xlim}{limits on x axis.}
    \item{ylim}{limits on y axis.}
    \item{zlim}{either a pair of numbers giving the limits for the colorscale,
        or \code{"histogram"} to have a flattened histogram (i.e. to maximally
        increase contrast throughout the domain.)}

    \item{decimate}{value indicating whether to decimate the image.  If
        \code{decimate=FALSE}, no decimation is done, i.e. all data are shown.
        If \code{decimate=TRUE}, then the image is decimated to display at most
        800 pixels in width or height.  If \code{decimate} is an integer, then
        its value sets the decimation interval, e.g. \code{decimate=20} plots
        every twentith point (suitable, for example, for landsat images that
        typicaly have 16000 pixels on a side).}

    \item{zclip}{logical, indicating whether to clip the colors to those
        corresponding to \code{zlim}. This only works if \code{zlim} is
        provided. Clipped regions will be colored with \code{missingColor}.
        Thus, clipping an image is somewhat analogous to clipping in an xy
        plot, with clipped data being ignored, which in an image means to be be
        colored with \code{missingColor}.}

    \item{flipy}{logical, with \code{TRUE} indicating that the image
        should be flipped top to bottom (e.g. to produce a profile image
        for a downward-looking acoustic-doppler profile).}
    \item{xlab,ylab,zlab}{names for x axis, y axis, and the image values.}
    \item{zlabPosition}{string indicating where to put the label for the z axis,
        either at the top-right of the main image, or on the side, in the axis
        for the palette.}
    \item{breaks}{the z values for breaks in the color scheme.  If this is of
        length 1, the value indicates the desired number of breaks, which is
        supplied to \code{\link{pretty}}, in determining clean break points.}
    \item{col}{either a vector of colors corresponding to the breaks, of length
        1 plus the number of breaks, or a function specifying colors,
        e.g. \code{\link{oce.colorsJet}} for a rainbow.}
    \item{colormap}{a color map as created by \code{\link{colormap}}.  If
        provided, then \code{colormap$breaks} and \code{colormap$col} take
        precedence over the present arguments \code{breaks} and \code{col}.
        (All of the other contents of \code{colormap} are ignored, though.)}
    \item{labels}{optional vector of labels for ticks on palette axis (must
        correspond with \code{at})}
    \item{at}{optional vector of positions for the \code{label}s}
    \item{drawContours}{logical value indicating whether to draw contours on the
        image, and palette, at the color breaks.  Images with a great deal of
        high-wavenumber variation look poor with contours.}
    \item{tformat}{optional argument passed to \code{\link{oce.plot.ts}}, for
        plot types that call that function.  (See \code{\link{strptime}} for the
        format used.)}
    \item{drawTimeRange}{logical, only used if the \code{x} axis is a
        time.  If \code{TRUE}, then an indication of the time range of the
        data (not the axis) is indicated at the top-left margin of the
        graph.  This is useful because the labels on time axes only indicate
        hours if the range is less than a day, etc.}
    \item{drawPalette}{indication of the type of palette to draw, if any.  If
        \code{drawPalette=TRUE}, a palette is drawn at the right-hand side of the
        main image.  If \code{drawPalette=FALSE}, no palette is drawn, and the
        right-hand side of the plot has a thin margin.  If
        \code{drawPalette="space"}, then no palette is drawn, but space is put in
        the right-hand margin to occupy the region in which the palette would
        have been drawn.  This last form is useful for producing stacked plots
        with uniform left and right margins, but with palettes on only some of
        the images.}
    \item{drawTriangles}{logical value indicating whether to draw
        triangles on the top and bottom of the palette.  This is passed to
        \code{\link{drawPalette}}.}
    \item{filledContour}{boolean value indicating whether to use filled
        contours to plot the image.}
    \item{missingColor}{a color to be used to indicate missing data, or
        \code{NULL} to avoid making the indication.}
    \item{mgp}{3-element numerical vector to use for \code{par(mgp)}, and
        also for \code{par(mar)}, computed from this.  The default is
        tighter than the R default, in order to use more space for the
        data and less for the axes.}
    \item{mar}{value to be used with \code{\link{par}("mar")}.  If not given, a
        reasonable value is calculated based on whether \code{xlab} and
        \code{ylab} are empty strings.}
    \item{mai.palette}{palette margin corrections (in inches), added to the
        \code{mai} value used for the palette.  Use with care.}
    \item{xaxs}{character indicating whether image should extend to edge
        of x axis (with value \code{"i"}) or not; see
        \code{\link[graphics]{par}}("xaxs").}
    \item{yaxs}{as \code{xaxs} but for y axis.}
    \item{cex}{size of labels on axes and palette; see \code{\link[graphics]{par}}("cex").}
    \item{adorn}{optional \code{\link{expression}} to be performed immediately after
        drawing the data panel.}     
    \item{axes}{logical, set \code{TRUE} to get axes on the main image.}
    \item{main}{title for plot.}
    \item{axisPalette}{optional replacement function for \code{axis()}, passed to
        \code{\link{drawPalette}}.}
    \item{debug}{a flag that turns on debugging.  Set to 1 to get a
        moderate amount of debugging information, or to 2 to get more.} 
    \item{\dots}{optional arguments passed to plotting functions.}
}

\details{Creates an image with a color palette to the right.  The effect is
    similar to \code{\link{filled.contour}} except that with \code{imagep} it
    is possible to set the \code{\link{layout}} outside the function, which
    enables the creation of plots with many image-palette panels.  Note that
    the contour lines may not coincide with the color transitions, in the case
    of coarse images.

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

\value{A list is silently returned, containing \code{xat} and \code{yat},
    values that can be used by \code{\link{oce.grid}} to add a grid to the
    plot.}

\seealso{This uses \code{\link{drawPalette}}.}

\examples{
    library(oce)

    ## 1. simplest use
    imagep(volcano)

    ## 2. something oceanographic (internal-wave speed)
    h <- seq(0, 50, length.out=100)
    drho <- seq(1, 3, length.out=200)
    speed <- outer(h, drho, function(drho, h) sqrt(9.8 * drho * h / 1024))
    imagep(h, drho, speed, xlab="Equivalent depth [m]",
    ylab=expression(paste(Delta*rho, " [kg/m^3]")),
    zlab="Internal-wave speed [m/s]")

    ## 3. fancy labelling on atan() function
    x <- seq(0, 1, 0.01)
    y <- seq(0, 1, 0.01)
    angle <- outer(x,y,function(x,y) atan2(y,x))
    imagep(x, y, angle, filledContour=TRUE, breaks=c(0, pi/4, pi/2),
           col=c("lightgray", "darkgray"),
           at=c(0, pi/4, pi/2),
           labels=c(0, expression(pi/4), expression(pi/2)))

    ## 4. a colormap case
    data(topoWorld)
    cm <- colormap(name="gmt_globe")
    imagep(topoWorld, colormap=cm)
}

\author{Dan Kelley, with help from Clark Richards}

\keyword{misc}

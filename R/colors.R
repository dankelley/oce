# vim: tw=120 shiftwidth=4 softtabstop=4 wrap linebreak expandtab:


#' Data that define some color palettes
#'
#' The `ocecolors` dataset is a list containing color-schemes, used
#' by [oceColorsClosure()] to create functions such as
#' [oceColorsViridis()].
#'
#' @name ocecolors
#'
#' @docType data

#' @author Authored by matplotlib contributers, packaged (with license permission) in oce by Dan Kelley
#'
#' @source The data come from the matplotlib site
#' \url{https://github.com/matplotlib/matplotlib}.
#' @family datasets provided with oce
#' @family things related to colors
NULL



colormapNames <- c("gmt_relief", "gmt_ocean", "gmt_globe", "gmt_gebco")

## keeping this (which was called 'colorize' until 2014-05-07) for a while, but not in NAMESPACE.
colormap_colorize <- function(z=NULL,
                              zlim, zclip=FALSE,
                              breaks, col=oceColorsViridis, colormap=NULL, segments=1,
                              missingColor="gray",
                              debug=getOption("oceDebug"))
{
    oceDebug(debug, "colormap_colorize(z=",
             if (is.null(z)) "(missing)" else paste("c(", z[1], ",...)", sep=""), ",",
             "zlim=", if (missing(zlim)) "(missing)" else
                 paste("c(", zlim[1], ",", zlim[2], "),", sep=""),
             "zclip=", zclip, ",",
             "breaks=", if (missing(breaks)) "(missing)" else
                 paste("c(", breaks[1], ",...),", sep=""),
             "col=", if (is.function(col)) "(function)" else paste("c(", col[1], ",...)", sep=""), ",",
             "colormap=", if (is.null(colormap)) "(missing)" else colormap, ",",
             "segments=", segments, ",",
             "missingColor=", missingColor,
             ") { # an internal function\n", sep="", unindent=1)
    if (is.null(colormap)) {
        if (is.function(col)) {
            oceDebug(debug, "col is a function\n")
            if (missing(breaks)) {
                if (!missing(zlim)) {
                    breaks <- seq(zlim[1], zlim[2], length.out=200)
                } else if (!is.null(z)) {
                    breaks <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=200)
                } else {
                    stop("must give z, zlim or breaks")
                }
            }
            if (length(breaks) == 1) {
                ## special case: 'breaks' means *number* of breaks
                if (!missing(zlim) && !is.null(zlim)) {
                    breaks <- seq(zlim[1], zlim[2], length.out=breaks)
                    ##message("pretty(zlim)")
                } else if (!is.null(z)) {
                    breaks <- pretty(z, n=breaks) # note use of pretty(), which extends from data
                    ##message("pretty(z)")
                } else {
                    stop("must give z or zlim if length(breaks)=1")
                }
            }
            col <- col(length(breaks) - 1)
        } else {
            ## FIXME: should check that it is indeed a color, but how?
            col <- col
        }
        if (is.null(z)) {
            oceDebug(debug, "z is NULL\n")
            if (missing(zlim) || is.null(zlim))
                zlim <- range(breaks)
            zcol <- "black"
        } else {
            oceDebug(debug, "z is not NULL\n")
            if (missing(zlim) || is.null(zlim)) {
                zlim <- rangeExtended(z) # note the extended range
                oceDebug(debug, "zlim not given; calculated as:", paste(zlim, collapse=" "), "\n")
            }
            i <- findInterval(z, breaks)
            tooLow <- i == 0
            tooHigh <- i == length(breaks)
            i[tooLow] <- 1             # cannot index to 0; col is replaced later
            zcol <- col[i]
            if (zclip) {
                oceDebug(debug, "zclip TRUE: out-of-range get missingColor:", missingColor, "\n")
                zcol[tooLow] <- missingColor
                zcol[tooHigh] <- missingColor
            } else {
                oceDebug(debug, "zclip FALSE: out-of-range get end colors\n")
                zcol[tooLow] <- col[1]
                zcol[tooHigh] <- tail(col, 1)
           }
        }
    } else {
        ## have a colormap
        if (!missing(col))
            stop("cannot supply 'col' and 'colormap' at the same time")
        if (!missing(breaks))
            stop("cannot supply 'breaks' and 'colormap' at the same time")
        if (is.character(colormap)) {
            colormap <- colormap_colormap(name=colormap, debug=debug-1)
            if (missing(zlim)) {
                zlim <- colormap$zlim
            }
            missingColor <- colormap$missingColor
        }
        if (inherits(colormap, "colormap"))
            missingColor <- colormap$missingColor
        breaks <- NULL
        col <- NULL
        ## Could preallocate but colormaps are small so do not bother
        n <- length(colormap$x0)
        for (i in seq.int(1, n-1)) {
            ## FIXME: should below be segments or 1+segments?
            delta <- (colormap$x0[i+1] - colormap$x0[i]) / segments
            breaks <- c(breaks, seq(from=colormap$x0[i], by=delta, length.out=segments))
            col <- c(col, colorRampPalette(c(colormap$col0[i], colormap$col1[i]))(segments))
        }
        nbreaks <- length(breaks)
        ## extend a bit to the right
        delta <- mean(diff(breaks[1:2])) / 1000
        breaks <- c(breaks, breaks[nbreaks] + delta)
        ## FIXME: next might miss top color
        if (is.null(zlim)) {
            if (is.null(z)) {
                zlim <- range(breaks)
            } else {
                zlim <- rangeExtended(z) # note the extended range
            }
        }
        if (is.null(z)) {
            zcol <- "black"
        } else {
            zlim <- rangeExtended(z)
            i <- findInterval(z, breaks)
            tooLow <- i == 0
            tooHigh <- i == length(breaks)
            i[tooLow] <- 1             # cannot index to 0; col is replaced later
            zcol <- col[i]
            if (zclip) {
                oceDebug(debug, "zclip TRUE: out-of-range get missingColor:", missingColor, "\n")
                zcol[tooLow] <- missingColor
                zcol[tooHigh] <- missingColor
            } else {
                oceDebug(debug, "zclip FALSE: out-of-range get end colors\n")
                zcol[tooLow] <- col[1]
                zcol[tooHigh] <- tail(col, 1)
           }
        }
    }
    res <- list(zlim=zlim, breaks=breaks, col=col, zcol=zcol, missingColor=missingColor)
    class(res) <- c("list", "colormap")
    oceDebug(debug, "} # colormap_colorize(), an internal function\n", sep="", unindent=1)
    res
}

## NB: I've not documented this, because it is not in the NAMESPACE.
colormapGmtNumeric <- function(x0, x1, col0, col1, bpl=1)
{
    n <- length(x0)
    if (length(x1) != n)
        stop("mismatched lengths of x0 and x1 (", n, " and ", length(x1), ")")
    if (length(col0) != n)
        stop("mismatched lengths of x0 and col0 (", n, " and ", length(col0), ")")
    if (length(col1) != n)
        stop("mismatched lengths of x0 and col1 (", n, " and ", length(col1), ")")
    breaks <- NULL
    col <- NULL
    ## Could preallocate but colormaps are small so do not bother
    for (i in seq.int(1, n-1)) {
        breaks <- c(breaks, seq(x0[i], x1[i], length.out=1+bpl))
        col <- c(col, colorRampPalette(c(col0[i], col1[i]))(1+bpl))
    }
    nbreaks <- length(breaks)
    ## extend a bit to the right
    delta <- mean(diff(breaks[1:2])) / 1000
    breaks <- c(breaks, breaks[nbreaks] + delta)
    res <- list(breaks=breaks, col=col)
    res
}

#>> #' Read a colormap from a GMT-type file
#>> #'
#>> #' Files of the GMT type may be found at
#>> #' \url{https://beamreach.org/maps/gmt/share/cpt/}, and consist
#>> #' of one or more lines starting with the `#` character, followed by a sequence
#>> #' of lines containing 8 numbers that, taken together, describe the color
#>> #' scheme, followed by a line with the character `F` followed by three integers
#>> #' giving red, green and blue values in the range 0 to 255, followed
#>> #' by a similar line starting with the character `B`, and finally by a line starting
#>> #' with the character `N`.  The last two lines are ignored, and the one before
#>> #' that is taken as the missing-value color.  As for the lines of 8 numbers, these
#>> #' each consist of two sequences of 4 numbers, in which the first is a value for
#>> #' the value being colored, and the others are red, green and blue values in the
#>> #' range from 0 to 255.
#>> #'
#>> #' @param text character value specifying a colormap, in GMT format.
#>> #'
#>> #' @param debug integer that, if positive, indicates to print some debugging output
#>> #'
#>> #' @return a list, in the same format as the return value for [colormap()].
#>> #'
#>> #' @author Dan Kelley
#>> #'
#>> #' @family things related to colors
#>> colormap_OLD_DELETE <- function(name, debug=getOption("oceDebug"))
#>> {
#>>     oceDebug(debug, "colormap_OLD_DELETE(\"", name, "\", debug=", debug, ")\n", sep="", unindent=1)
#>>     if (missing(name))
#>>         stop("must give 'name'\n")
#>>     if (!is.character(name))
#>>         stop("'name' must be a character value")
#>>     text <- readLines(filename)
#>>     textData <- text[grep("^[ ]*[-0-9]", text)]
#>>     textData <- gsub("/", " ", textData) # sometimes it is R/G/B
#>>     d <- read.table(text=textData, col.names=c("x0", "r0", "g0", "b0", "x1", "r1", "g1", "b1"))
#>>     col0 <- rgb(d$r0, d$g0, d$b0, maxColorValue=255)
#>>     col1 <- rgb(d$r1, d$g1, d$b1, maxColorValue=255)
#>>     ## Decode (F, B) and N=missingColor.  Note step by step approach,
#>>     ## which may be useful in debugging different formats, e.g.
#>>     ## tabs and spaces etc.
#>>     ## "F" unused at present
#>>     F <- "#FFFFFF"
#>>     if (length(grep("^\\sF", text))) {
#>>         line <- text[grep("\\s*F", text)]
#>>         line <- gsub("^\\sF", "", line)
#>>         F <- scan(text=line, quiet=TRUE)
#>>         Flen <- length(F)
#>>         if (1 == Flen) {
#>>             F <- if (length(grep("[a-zA-Z]", F))) F else gray(as.numeric(F) / 255)
#>>         } else if (3 == Flen) {
#>>             F <- rgb(F[1], F[2], F[3], maxColorValue=255)
#>>         } else {
#>>             warning("cannot decode \"F\" from \"", line, "\"")
#>>         }
#>>     }
#>>     ## "B" unused at present
#>>     B <- "#000000"
#>>     if (length(grep("^\\sB", text))) {
#>>         line <- text[grep("\\s*B", text)]
#>>         line <- gsub("^\\sB", "", line)
#>>         B <- scan(text=line, quiet=TRUE)
#>>         Blen <- length(B)
#>>         if (1 == Blen) {
#>>             B <- if (length(grep("[a-zA-Z]", B))) B else gray(as.numeric(B) / 255)
#>>         } else if (3 == Blen) {
#>>             B <- rgb(B[1], B[2], B[3], maxColorValue=255)
#>>         } else {
#>>             warning("cannot decode \"B\" from \"", line, "\"")
#>>         }
#>>     }
#>>     ## "N" named here as missingColor to match e.g. imagep()
#>>     N <- "gray"
#>>     if (length(grep("^\\s*N", text))) {
#>>         line <- text[grep("^\\s*N", text)]
#>>         line <- gsub("\\s*N", "", line)
#>>         N <- scan(text=line, what=character(), quiet=TRUE)
#>>         Nlen <- length(N)
#>>         if (1 == Nlen) {
#>>             N <- if (length(grep("[a-zA-Z]", N))) N else gray(as.numeric(N) / 255)
#>>         } else if (3 == Nlen) {
#>>             N <- rgb(N[1], N[2], N[3], maxColorValue=255)
#>>         } else {
#>>             warning("cannot decode missingColor from \"", line, "\"")
#>>         }
#>>     }
#>>     res <- list(x0=d$x0, x1=d$x1, col0=col0, col1=col1, missingColor=N)
#>>     class(res) <- c("list", "colormap")
#>>     oceDebug(debug, "} # colormapFromFile()\n", sep="", unindent=1)
#>>     res
#>> }

#' Create a GMT-type colormap
#'
#' `colormapGMT` creates colormaps in the Generic Mapping Tools (GMT)
#' scheme (see References 1 and 2).  A few such schemes are built-in, and may be referred to
#' by name (`"gmt_gebco"`, `"gmt_globe"`, `"gmt_ocean"`, or `"gmt_relief"`)
#' while others are handled by reading local files that are in GMT
#' format, or URLs providing such files (see Reference 3).
#'
#' GMT files start with optional comment lines that begin with
#' the `#` character, followed by a sequence of lines containing 8 numbers,
#' followed by a line with the character `F` followed by three integers
#' giving red, green and blue values in the range 0 to 255, followed
#' by a similar line starting with the character `B`,
#' and finally by a similar line starting
#' with the character `N`. The last line
#' is interpreted as the missing-value color.  The lines containing 8 numbers each
#' describe a color band. The first number is the minimum value in the band,
#' and this is followed by integers in the range 0:255 that specify the
#' red, green and blue components of the color.  The fourth to eight numbers
#' similarly describe the upper end of that color band.
#'
#' @param name character value specifying the GMT scheme, or a source for such
#' a scheme. Four pre-defined schemes are available, accessed by setting `name` to
#' `"gmt_gebco"`, `"gmt_globe"`, `"gmt_ocean"`, or `"gmt_relief"`. If `name` is
#' not one of these values, then it is taken to be the name of a local file
#' in GMT format or, if no such file is found, a URL holding such a file.
#'
#' @param debug integer that, if positive, indicates to print some debugging output
#'
#' @return `colormap` returns a list, in the same format as the return value for [colormap()].
#'
#' @author Dan Kelley
#'
#' @references
#' 1. General overview of GMT system
#' <https://www.generic-mapping-tools.org>.
#' 2. Information on GMT color schemes
#' <https://docs.generic-mapping-tools.org/dev/cookbook/cpts.html>
#' 3. Source of GMT specification files
#' <https://beamreach.org/maps/gmt/share/cpt>
#'
#' @family things related to colors
colormapGMT <- function(name, debug=getOption("oceDebug"))
{
    oceDebug(debug, "colormapGMT(name=\"", name, "...)\n", sep="", unindent=1)
    if (name %in% colormapNames) {
        oceDebug(debug, "case 1: built-in GMT name\n")
        if (name == "gmt_relief") {
            oceDebug(debug, "case 1.1: built-in gmt_relief\n")
            ## $Id: GMT_relief.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
            ##
            ## Colortable for whole earth relief used in Wessel topomaps
            ## Designed by P. Wessel and F. Martinez, SOEST
            ## COLOR_MODEL = RGB
            text <- "-8000   0       0       0       -7000   0       5       25
            -7000   0       5       25      -6000   0       10      50
            -6000   0       10      50      -5000   0       80      125
            -5000   0       80      125     -4000   0       150     200
            -4000   0       150     200     -3000   86      197     184
            -3000   86      197     184     -2000   172     245     168
            -2000   172     245     168     -1000   211     250     211
            -1000   211     250     211     0       250     255     255
            0       70      120     50      500     120     100     50
            500     120     100     50      1000    146     126     60
            1000    146     126     60      2000    198     178     80
            2000    198     178     80      3000    250     230     100
            3000    250     230     100     4000    250     234     126
            4000    250     234     126     5000    252     238     152
            5000    252     238     152     6000    252     243     177
            6000    252     243     177     7000    253     249     216
            7000    253     249     216     8000    255     255     255
            F       255     255     255
            B       0       0       0
            N       255     255     255"
        } else if (name == "gmt_ocean") {
            oceDebug(debug, "case 1.2: built-in gmt_ocean\n")
            ## $Id: GMT_ocean.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
            ##
            ## Colortable for oceanic areas as used in Wessel maps
            ## Designed by P. Wessel and F. Martinez, SOEST.
            ## COLOR_MODEL = RGB
            text <- "-8000   0       0       0       -7000   0       5       25
            -7000   0       5       25      -6000   0       10      50
            -6000   0       10      50      -5000   0       80      125
            -5000   0       80      125     -4000   0       150     200
            -4000   0       150     200     -3000   86      197     184
            -3000   86      197     184     -2000   172     245     168
            -2000   172     245     168     -1000   211     250     211
            -1000   211     250     211     0       250     255     255
            F       255     255     255
            B       0       0       0"
        } else if (name == "gmt_globe") {
            oceDebug(debug, "case 1.3: built-in gmt_globe\n")
            ##       $Id: GMT_globe.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
            ##
            ## Colormap using in global relief maps
            ## Bathymetry colors manually redefined for blue-shade effect and
            ## new topography color scheme for use with Generic Mapping Tools.
            ## Designed by Lester M. Anderson (CASP, UK) lester.anderson@casp.cam.ac.uk
            ## COLOR_MODEL = RGB
            text <- "-10000  153     0       255     -9500   153     0       255
            -9500   153     0       255     -9000   153     0       255
            -9000   153     0       255     -8500   153     0       255
            -8500   136     17      255     -8000   136     17      255
            -8000   119     34      255     -7500   119     34      255
            -7500   102     51      255     -7000   102     51      255
            -7000   85      68      255     -6500   85      68      255
            -6500   68      85      255     -6000   68      85      255
            -6000   51      102     255     -5500   51      102     255
            -5500   34      119     255     -5000   34      119     255
            -5000   17      136     255     -4500   17      136     255
            -4500   0       153     255     -4000   0       153     255
            -4000   27      164     255     -3500   27      164     255
            -3500   54      175     255     -3000   54      175     255
            -3000   81      186     255     -2500   81      186     255
            -2500   108     197     255     -2000   108     197     255
            -2000   134     208     255     -1500   134     208     255
            -1500   161     219     255     -1000   161     219     255
            -1000   188     230     255     -500    188     230     255
            -500    215     241     255     -200    215     241     255
            -200    241     252     255     0       241     252     255
            0       51      102     0       100     51      204     102
            100     51      204     102     200     187     228     146
            200     187     228     146     500     255     220     185
            500     255     220     185     1000    243     202     137
            1000    243     202     137     1500    230     184     88
            1500    230     184     88      2000    217     166     39
            2000    217     166     39      2500    168     154     31
            2500    168     154     31      3000    164     144     25
            3000    164     144     25      3500    162     134     19
            3500    162     134     19      4000    159     123     13
            4000    159     123     13      4500    156     113     7
            4500    156     113     7       5000    153     102     0
            5000    153     102     0       5500    162     89      89
            5500    162     89      89      6000    178     118     118
            6000    178     118     118     6500    183     147     147
            6500    183     147     147     7000    194     176     176
            7000    194     176     176     7500    204     204     204
            7500    204     204     204     8000    229     229     229
            8000    229     229     229     8500    242     242     242
            8500    242     242     242     9000    255     255     255
            9000    255     255     255     9500    255     255     255
            9500    255     255     255     10000   255     255     255
            F       255     255     255
            B       0       0       0
            N       128     128     128"
        } else if (name == "gmt_gebco") {
            oceDebug(debug, "case 1.4: built-in gmt_gebco\n")
            ## $Id: GMT_gebco.cpt,v 1.1.1.1 2000/12/28 01:23:45 gmt Exp $
            ##
            ## Bathymetry colors approximating the GEBCO charts
            ## Designed by Andrew Goodwillie, Scripps
            ## COLOR_MODEL = RGB
            text <- "-7000   0       240     255     -6000   0       240     255
            -6000   35      255     255     -5000   35      255     255
            -5000   90      255     255     -4000   90      255     255
            -4000   140     255     230     -3000   140     255     230
            -3000   165     255     215     -2000   165     255     215
            -2000   195     255     215     -1000   195     255     215
            -1000   210     255     215     -500    210     255     215
            -500    230     255     240     -200    230     255     240
            -200    235     255     255     -0      235     255     255
            F       255     255     255
            B       0       0       0
            N       128     128     128"
        }
        text <- gsub("^[ ]*", "", strsplit(text, "\\n")[[1]])
    } else {
        oceDebug(debug, "case 2: file or URL\n")
        # Look for local file or URL
        text <- try(readLines(name), silent=TRUE)
        if (inherits(text, "try-error"))
            stop("unknown colormap name: \"", name, "\" (not built-in, not local file, not working URL)")
    }
    text <- text[!grepl("^#", text)] # remove comments, if any exist (as in files read in)
    textData <- text[grep("^[ ]*[-0-9]", text)]
    textData <- gsub("/", " ", textData) # sometimes it is R/G/B
    d <- read.table(text=textData, col.names=c("x0", "r0", "g0", "b0", "x1", "r1", "g1", "b1"))
    col0 <- rgb(d$r0, d$g0, d$b0, maxColorValue=255)
    col1 <- rgb(d$r1, d$g1, d$b1, maxColorValue=255)
    ## Decode (F, B) and N=missingColor.  Note step by step approach,
    ## which may be useful in debugging different formats, e.g.
    ## tabs and spaces etc.
    ## "F" unused at present
    F <- "#FFFFFF"
    if (length(grep("^\\sF", text))) {
        line <- text[grep("\\s*F", text)]
        line <- gsub("^\\sF", "", line)
        F <- scan(text=line, quiet=TRUE)
        Flen <- length(F)
        if (1 == Flen) {
            F <- if (length(grep("[a-zA-Z]", F))) F else gray(as.numeric(F) / 255)
        } else if (3 == Flen) {
            F <- rgb(F[1], F[2], F[3], maxColorValue=255)
        } else {
            warning("cannot decode \"F\" from \"", line, "\"")
        }
    }
    ## "B" unused at present
    B <- "#000000"
    if (length(grep("^\\sB", text))) {
        line <- text[grep("\\s*B", text)]
        line <- gsub("^\\sB", "", line)
        B <- scan(text=line, quiet=TRUE)
        Blen <- length(B)
        if (1 == Blen) {
            B <- if (length(grep("[a-zA-Z]", B))) B else gray(as.numeric(B) / 255)
        } else if (3 == Blen) {
            B <- rgb(B[1], B[2], B[3], maxColorValue=255)
        } else {
            warning("cannot decode \"B\" from \"", line, "\"")
        }
    }
    ## "N" named here as missingColor to match e.g. imagep()
    N <- "gray"
    if (length(grep("^\\s*N", text))) {
        line <- text[grep("^\\s*N", text)]
        line <- gsub("\\s*N", "", line)
        N <- scan(text=line, what=character(), quiet=TRUE)
        Nlen <- length(N)
        if (1 == Nlen) {
            N <- if (length(grep("[a-zA-Z]", N))) N else gray(as.numeric(N) / 255)
        } else if (3 == Nlen) {
            N <- rgb(N[1], N[2], N[3], maxColorValue=255)
        } else {
            warning("cannot decode missingColor from \"", line, "\"")
        }
    }
    col <- head(rgb(d$r0, d$g0, d$b0, maxColorValue=255L), -1L)
    #colbreaks <- colormapGmtNumeric(d$x0, d$x1, d$col0, d$col1)
    res <- list(x0=d$x0, x1=d$x1, col0=col0, col1=col1, breaks=d$x0, col=col, missingColor=N)
    class(res) <- c("list", "colormap")
    oceDebug(debug, "} # colormapGMT()\n", sep="", unindent=1)
    res
}

#' Calculate color map
#'
#' Map values to colors, for use in palettes and plots. There are many ways to
#' use this function, and some study of the arguments should prove fruitful in
#' cases that extend far beyond the examples.
#'
#' This is a multi-purpose function that generally links (``maps'') numerical
#' values to colors.  The return value can specify colors for points on a
#' graph, or `breaks` and `col` vectors that are suitable for use by
#' [drawPalette()], [imagep()] or [image()].
#'
#' There are several ways of specifying color schemes, of which
#' the following are common.
#'
#' * **Case A.** Supply some combination of arguments that
#' is sufficient to define a mapping of value to colour, without
#' providing `x0`, `col0`, `x1` or `col1` (see case B for these),
#' or providing `name` (see Case C). There are several ways to
#' do this.  A common approach is to supply only `z` but no
#' other argument, in which case `zlim`, and `breaks` will be determined
#' from `z`, and the default `col` will be used.  Another approach is
#' to specify `breaks` and `col` together, in the same way as they
#' might be specified for the base R function [image()].  It is
#' also possible to supply only `zlim`, in which case `breaks` is
#' inferred from that value.
#' \if{html}{The figure below explains the
#' (`breaks`, `col`) method of specifying a color mapping.  Note
#' that there must be one more break than color.  This is the method used by
#' e.g. [image()].}
#' \if{html}{\figure{colormap_fig_1.png}}
#'
#' * **Case B.** Supply `x0`, `col0`, `x1`, and `col1`, but *not*
#' `zlim`, `breaks`, `col` or `name`.
#' The `x0`, `col0`, `x1` and `col1` values specify a
#' value-color mapping that is similar to that used
#' for GMT color maps.  The method works by using [seq()] to
#' interpolate between the elements of the `x0` vector.  The same is done
#' for `x1`.  Similarly, [colorRampPalette()] is used to
#' interpolate between the colors in the `col0` vector, and the same is
#' done for `col1`.  \if{html}{The figure above explains the (`x0`,
#' `x1`, `col0`, `col1`) method of specifying a color mapping.
#' Note that the each of the items has the same length. The case of
#' `blend=0`, which has color `col0[i]` between `x0[i]` and
#' `x1[i]`, is illustrated below.}
#' \if{html}{\figure{colormap_fig_2.png}}
#'
#' * **Case C.** Supply `name` and possibly also `z`, but *not*
#' `zlim`, `breaks`, `col`, `x0`, `col0`, `x1` or `col1`.
#' The `name` may be the name of a pre-defined color palette
#' (`"gmt_relief"`, `"gmt_ocean"`, `"gmt_globe"` or
#' `"gmt_gebco"`), or it may be the name of a file (or URL pointing to a file)
#' that contains a color map in the GMT format (see \dQuote{References}). If
#' `z` is supplied along with `name`, then `zcol` will be set up in the
#' return value, e.g. for use in colourizing points.  Another method
#' for finding colours for data points is to use the `colfunction()`
#' function in the return value.
#'
#' @param z an optional vector or other set of numerical values to be examined.
#' If `z` is given, the return value will contain an item named
#' `zcol` that will be a vector of the same length as `z`, containing
#' a color for each point.  If `z` is not given, `zcol` will contain
#' just one item, the color `"black"`.
#'
#' @param zlim optional vector containing two numbers that specify the `z`
#' limits for the color scale.  If provided, it overrides defaults as describe
#' in the following.  If `name` is given, then the [range()] of
#' numerical values contained therein will be used for `zlim`.  Otherwise,
#' if `z` is given, then its [rangeExtended()] sets `zlim`.
#' Otherwise, if `x0` and `x1` are given, then their
#' [range()] sets `zlim`.  Otherwise, there is no way to infer
#' `zlim` and indeed there is no way to construct a colormap, so an error
#' is reported.  It is an error to specify both `zlim` and `breaks`,
#' if the length of the latter does not equal 1.
#'
#' @param zclip logical, with `TRUE` indicating that z values outside the
#' range of `zlim` or `breaks` should be painted with
#' `missingColor` and `FALSE` indicating that these values should be
#' painted with the nearest in-range color.
#'
#' @param breaks an optional indication of break points between color levels
#' (see [image()]).  If this is provided, the arguments `name`
#' through `blend` are all ignored (see \dQuote{Details}).  If it is
#' provided, then it may either be a vector of break points, or a single number
#' indicating the desired number of break points to be computed with
#' [`pretty`]`(z,breaks)`.  In either case of non-missing
#' `breaks`, the resultant break points must number 1 plus the number of
#' colors (see `col`).
#'
#' @param col either a vector of colors or a function taking a numerical value
#' as its single argument and returning a vector of colors.  Prior to 2021-02-08,
#' the default for `col` was `oceColorsJet`, but it was switched to
#' `oceColorsViridis` on that date.  The value of
#' `col` is ignored if `name` is provided, or if `x0` through
#' `col1` are provided.
#'
#' @param name an optional string naming a built-in colormap (one of
#' `"gmt_relief"`, `"gmt_ocean"`, `"gmt_globe"` or
#' `"gmt_gebco"`) or the name of a file or URL that contains a color map
#' specification in GMT format.  If `name` is given, then it is
#' passed to [colormapGMT()], which creates the colormap.
#' Note that the colormap thus created has a fixed
#' relationship between value and color, and `zlim`,
#` `z0`, etc. are ignored when `name` is provided. Indeed, the
#' only other argument that is examined is `z` (which may be used
#' so that `zcol` will be defined in the return value), and warnings
#' are issued if some irrelevant arguments are provided.
#'
#' @param x0,x1,col0,col1 Vectors that specify a color map.  They must all be
#' the same length, with `x0` and `x1` being numerical values, and
#' `col0` and `col1` being colors.  The colors may be strings (e.g.
#' `"red"`) or colors as defined by [rgb()] or [hsv()].
#'
#' @param blend a number indicating how to blend colors within each band.
#' This is ignored except when `x0` through `col1` are supplied.  A
#' value of 0 means to use `col0[i]` through the interval `x0[i]` to
#' `x1[i]`.  A value of 1 means to use `col1[i]` in that interval.  A
#' value between 0 and 1 means to blend between the two colors according to
#' the stated fraction.  Values exceeding 1 are an error at present, but there
#' is a plan to use this to indicate subintervals, so a smooth palette can be
#' created from a few colors.
#'
#' @param missingColor color to use for missing values.  If not provided, this
#' will be `"gray"`, unless `name` is given, in which case it comes
#' from that color table.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @return a list containing the following (not necessarily in this order)
#'
#' * `zcol`, a vector of colors for `z`, if `z` was
#' provided, otherwise `"black"`
#'
#' * `zlim`, a two-element vector suitable as the argument of the same
#' name supplied to [image()] or [imagep()]
#'
#' * `breaks` and `col`, vectors of breakpoints and colors,
#' suitable as the same-named arguments to [image()] or
#' [imagep()]
#'
#' * `zclip` the provided value of `zclip`.
#'
#' * `x0` and `x1`, numerical vectors of the sides of color
#' intervals, and `col0` and `col1`, vectors of corresponding
#' colors.  The meaning is the same as on input.  The purpose of returning
#' these four vectors is to permit users to alter color mapping, as in example
#' 3 in \dQuote{Examples}.
#'
#' * `missingColor`, a color that could be used to specify missing
#' values, e.g. as the same-named argument to [imagep()].  If this is
#' supplied as an argument, its value is repeated in the return value.
#' Otherwise, its value is either `"gray"` or, in the case of `name`
#' being given, the value in the GMT color map specification.
#'
#' * `colfunction`, a univariate function that returns a vector
#' of colors, given a vector of `z` values; see Example 6.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' ## Example 1. color scheme for points on xy plot
#' x <- seq(0, 1, length.out=40)
#' y <- sin(2 * pi * x)
#' par(mar=c(3, 3, 1, 1))
#' mar <- par('mar') # prevent margin creep by drawPalette()
#' ## First, default breaks
#' c <- colormap(y)
#' drawPalette(c$zlim, col=c$col, breaks=c$breaks)
#' plot(x, y, bg=c$zcol, pch=21, cex=1)
#' grid()
#' par(mar=mar)
#' ## Second, 100 breaks, yielding a smoother palette
#' c <- colormap(y, breaks=100)
#' drawPalette(c$zlim, col=c$col, breaks=c$breaks)
#' plot(x, y, bg=c$zcol, pch=21, cex=1)
#' grid()
#' par(mar=mar)
#'
#'\dontrun{
#' ## Example 2. topographic image with a standard color scheme
#' par(mfrow=c(1,1))
#' data(topoWorld)
#' cm <- colormap(name="gmt_globe")
#' imagep(topoWorld, breaks=cm$breaks, col=cm$col)
#'
#' ## Example 3. topographic image with modified colors,
#' ## black for depths below 4km.
#' cm <- colormap(name="gmt_globe")
#' deep <- cm$x0 < -4000
#' cm$col0[deep] <- 'black'
#' cm$col1[deep] <- 'black'
#' cm <- colormap(x0=cm$x0, x1=cm$x1, col0=cm$col0, col1=cm$col1)
#' imagep(topoWorld, breaks=cm$breaks, col=cm$col)
#'
#' ## Example 4. image of world topography with water colorized
#' ## smoothly from violet at 8km depth to blue
#' ## at 4km depth, then blending in 0.5km increments
#' ## to white at the coast, with tan for land.
#' cm <- colormap(x0=c(-8000, -4000,   0,  100),
#'                x1=c(-4000,     0, 100, 5000),
#'                col0=c("violet","blue","white","tan"),
#'                col1=c("blue","white","tan","yellow"))
#' lon <- topoWorld[['longitude']]
#' lat <- topoWorld[['latitude']]
#' z <- topoWorld[['z']]
#' imagep(lon, lat, z, breaks=cm$breaks, col=cm$col)
#' contour(lon, lat, z, levels=0, add=TRUE)
#'
#' ## Example 5. visualize GMT style color map
#' cm <- colormap(name="gmt_globe", debug=4)
#' plot(seq_along(cm$x0), cm$x0, pch=21, bg=cm$col0)
#' grid()
#' points(seq_along(cm$x1), cm$x1, pch=21, bg=cm$col1)
#'
#' ## Example 6. colfunction
#' cm <- colormap(c(0, 1))
#' x <- 1:10
#' y <- (x - 5.5)^2
#' z <- seq(0, 1, length.out=length(x))
#' drawPalette(colormap=cm)
#' plot(x, y, pch=21, bg=cm$colfunction(z), cex=3)
#'}
#'
#' @family things related to colors
colormap <- function(z=NULL,
                     zlim, zclip=FALSE,
                     breaks, col=oceColorsViridis,
                     name, x0, x1, col0, col1, blend=0,
                     missingColor,
                     debug=getOption("oceDebug"))
{
    debug <- min(max(0, debug), 1)
    #oceDebug(debug, gsub(" = [^,)]*", "", deparse(expr=match.call())), " {\n", style="bold", sep="", unindent=1)
    oceDebug(debug, "colormap(...) {\n", sep="", style="bold", unindent=1)
    zKnown <- !is.null(z)
    zlimKnown <- !missing(zlim)
    breaksKnown <- !missing(breaks)
    colKnown <- !missing(col)
    nameKnown <- !missing(name)
    x0Known <- !missing(x0)
    x1Known <- !missing(x1)
    col0Known <- !missing(col0)
    col1Known <- !missing(col1)
    missingColorKnown <- !missing(missingColor)
    # Find cases (as a way to clarify code, and link it with the docs).
    if (x0Known || col0Known || x1Known || col1Known) {
        case <- "B"
    } else if (nameKnown) {
        case <- "C"
    } else if (zKnown || zlimKnown || breaksKnown) {
        case <- "A"
    } else {
        case <- "A"
        warning("defaulting to case A, which may be incorrect\n")
    }

    # Case C: 'name' was given: only 'name' and possibly 'z' are examined.
    if (case == "C") {
        oceDebug(debug, "Case C: name given\n", style="bold")
        if (zlimKnown) warning("ignoring 'zlim', since 'name' was given (i.e. Case C)\n")
        if (breaksKnown) warning("ignoring 'breaks', since 'name' was given (i.e. Case C)\n")
        if (colKnown) warning("ignoring 'col', since 'name' was given (i.e. Case C)\n")
        if (x0Known) warning("ignoring 'x0', since 'name' was given (i.e. Case C)\n")
        if (col0Known) warning("ignoring 'col0', since 'name' was given (i.e. Case C)\n")
        if (x1Known) warning("ignoring 'x1', since 'name' was given (i.e. Case C)\n")
        if (col1Known) warning("ignoring 'col1', since 'name' was given (i.e. Case C)\n")
        if (missingColorKnown) warning("ignoring 'missingColor', since 'name' was given (i.e. Case C)\n")
        res <- colormap_colormap(name=name, debug=debug-1)
        res$zclip <- zclip
        res$zlim <- range(c(res$x0, res$x1)) # ignore argument 'zlim'
        res$colfunction <- function(z) res$col0[findInterval(z, res$x0, all.inside=TRUE)]
        if (zKnown)
            res$zcol <- res$colfunction(z)
        oceDebug(debug, "} # colormap()\n", style="bold", sep="", unindent=1)
        return(res)
    }
    # Sanity checks on blend and zlim
    if (blend < 0 || blend > 1)
        stop("blend must be between 0 and 1")
    if (zlimKnown) {
        if (length(zlim) != 2)
            stop("'zlim' must be of length 2")
        if (any(!is.finite(zlim)))
            stop("'zlim' values must be finite")
        if (zlim[2] <= zlim[1])
            stop("'zlim' values must be ordered and distinct")
    }
    if (missingColorKnown)
        oceDebug(debug, 'missingColor:', missingColor, '\n')
    if (case == "B") {
        oceDebug(debug, "Case B\n", style="bold")
        if (!(x0Known && col0Known && x1Known && col1Known))
            stop("'x0', 'col0', 'x1', 'col1' must all be supplied, if any is supplied")
        for (unwanted in c("name", "breaks", "col"))
            if (get(paste0(unwanted, "Known")))
                warning("'", unwanted, "' is ignored for case ", case, "\n", sep="")
        if (length(x0) != length(x1))
            stop("lengths of x0 and x1 must agree")
        if (length(col0) != length(col1))
            stop("lengths of col0 and col1 must agree")
        if (length(x0) != length(col0))
            stop("lengths of x0 and col0 must agree")
        breaks <- c(x0, tail(x1, 1))
        ## blend colors
        col <- col2rgb(col0) # will overwrite
        oceDebug(debug, "blend=", blend, "\n", sep="")
        for (i in seq_along(col0)) {
            col[, i] <- colorRamp(c(col0[i], col1[i]))(blend)[1, ]
        }
        col <- rgb(col[1, ], col[2, ], col[3, ], maxColorValue=255)
        if (!missingColorKnown)
            missingColor <- "gray"
        if (zKnown) {
            ## BOOKMARK1 -- this code needs to be in synch with BOOKMARK2
            i <- findInterval(z, breaks)
            missing <- i == 0
            i[missing] <- 1            # just pick something; replaced later
            zcol <- col[i]
            zcol[missing] <- missingColor
            if (zclip) {
                zcol[missing] <- missingColor
            } else {
                if (zKnown)
                    zcol[is.na(z)] <- missingColor
                zcol[z <= min(breaks)] <- col[1]
                zcol[z >= max(breaks)] <- tail(col, 1)
            }
        } else {
            zcol <- "black"
        }
        if (!zlimKnown) {
            #? bandwidth <- x1[1] - x0[1]
            #? if (bandwidth > 0) {
            #?     zlim <- c(min(x0) - bandwidth, max(x1) + bandwidth)
            #? } else {
            #?     zlim <- range(c(x0, x1))
            #? }
            zlim <- range(c(x0, x1))
        }
        res <- list(x0=x0, x1=x1, col0=col0, col1=col1,
                     missingColor=missingColor,
                     zclip=zclip,
                     zlim=zlim,
                     breaks=breaks,
                     col=col,
                     zcol=zcol)
        class(res) <- c("list", "colormap")
        oceDebug(debug, "} # colormap()\n", style="bold", sep="", unindent=1)
        return(res)
    }
    oceDebug(debug, "case 3: name not given, x0 (and related) not given\n")
    if (zlimKnown && breaksKnown && length(breaks) > 1)
        stop("cannot specify both zlim and breaks, unless length(breaks)==1")
    if (!zlimKnown) {
        if (breaksKnown) {
            oceDebug(debug, "zlimKnown=", zlimKnown, ", so inferring zlim from breaks\n", sep="")
            zlim <- if (length(breaks) > 1) range(breaks) else NULL
            zlimKnown <- TRUE
        } else if (zKnown) {
            oceDebug(debug, "zlimKnown=", zlimKnown, ", so inferring zlim from z\n", sep="")
            if (!any(is.finite(z)))
                stop("cannot infer zlim, since z has no finite values, and breaks was not given")
            zlim <- rangeExtended(z[is.finite(z)])
            zlimKnown <- TRUE
        } else  {
            stop("cannot infer zlim; please specify zlim, breaks, name, or z")
        }
    }
    oceDebug(debug, "zlim=", if (is.null(zlim)) "NULL" else zlim, "\n")
    oceDebug(debug, "zclip=", zclip, "\n")
    blend <- max(blend, 0L)
    n <- if (blend > 1L) as.integer(round(blend)) else 1L
    oceDebug(debug, "blend=", blend, "; n=", n, "\n", sep="")
    # Possibly determine breaks, if not given but if can be inferred from z or zlim
    if (!breaksKnown) {
        if (zlimKnown) {
            oceDebug(debug, "case 3.1 breaks not given, but inferred from zlim\n")
            breaks <- seq(min(zlim, na.rm=TRUE), max(zlim, na.rm=TRUE), length.out=255)
            oceDebug(debug, "created", length(breaks), "breaks, ranging from", breaks[1], "to", tail(breaks,1), "\n")
            breaksKnown <- TRUE            # this makes next block execute also
        } else if (zKnown && any(is.finite(z))) {
            oceDebug(debug, "case 3.2 breaks not given, but inferred from z\n")
            breaks <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=255)
            oceDebug(debug, "created", length(breaks), "breaks, ranging from", breaks[1], "to", tail(breaks,1), "\n")
            breaksKnown <- TRUE
        }
    }
    if (breaksKnown) {
        oceDebug(debug, "case 4 breaks given, or inferred from either z or zlim\n")
        if (zKnown) {
            oceDebug(debug, "case 4.1 construct colormap using z\n")
            if (length(breaks) < 2) {
                breaks <- seq(min(z, na.rm=TRUE), max(z, na.rm=TRUE), length.out=breaks)
                oceDebug(debug, "constructing breaks from z as ", paste(breaks, collapse=" "), "\n")
            }
            if (missing(missingColor)) {
                res <- colormap_colorize(zlim=zlim, zclip=zclip, z=z, breaks=breaks, col=col,
                                          debug=debug-1)
            } else {
                res <- colormap_colorize(zlim=zlim, zclip=zclip, z=z, breaks=breaks, col=col,
                                          missingColor=missingColor, debug=debug-1)
            }
        } else {
            oceDebug(debug, "case 4.2 construct colormap using zlim\n")
            oceDebug(debug, "length(breaks)=", length(breaks), "\n", sep="")
            if (length(breaks) < 2) {
                breaks <- seq(zlim[1], zlim[2], length.out=breaks)
                oceDebug(debug, "constructing breaks from zlim as ", paste(breaks, collapse=" "), "\n")
            }
            if (missing(missingColor)) {
                res <- colormap_colorize(zlim=zlim, zclip=zclip, breaks=breaks, col=col, debug=debug-1)
            } else {
                res <- colormap_colorize(zlim=zlim, zclip=zclip, breaks=breaks, col=col, missingColor=missingColor, debug=debug-1)
            }
            res$zcol <- "black"
        }
        res$x0 <- res$breaks[-1]
        res$x1 <- res$breaks[-1]
        res$col0 <- res$col
        res$col1 <- res$col
    }
    #*** # NOTE: 2021-04-04 this block was not executed, because now the 'name' case is handled above. Retain for a while,
    #*** # though.
    #*** else {
    #***     if (nameKnown) {
    #***         oceDebug(debug, "processing case C: 'name' was given\n")
    #***         res <- colormap_colormap(name=name, debug=debug-1)
    #***     } else {
    #***         ##if (x0Known) {
    #***         ##    oceDebug(debug, "processing case D: 'x0', 'x1', 'col0' and 'col1' were given, all of length",
    #***         ##             length(x0), "\n")
    #***         ##    res <- colormap_colormap(x0=x0, x1=x1, col0=col0, col1=col1, n=n, debug=debug-1)
    #***         ##    ## If n>1, we will have lots of levels, and will centre them
    #***         ##    if (n > 1L)
    #***         ##        blend <- 0.5
    #***         ##    oceDebug(debug, "length(col0)=", length(col0), "; length(res$col0)=", length(res$col0), "\n")
    #***         ##} else {
    #***         breaks <- pretty(z)
    #***         stop('must give "breaks" or "name", or each of "x0", "x1", "col0", and "col1"')
    #***         ##}
    #***     }
    #***     ## FIXME: issue 435 work in next 5 to 10 lines below
    #***     ##message("zlim: ", if (is.null(zlim)) "NULL" else paste(zlim, collapse=" to "))
    #***     res$zlim <- if (is.null(zlim)) range(c(res$x0, res$x1)) else zlim
    #***     ##nx0 <- length(res$x0)
    #***     ##eps <- diff(res$x0[1:2]) / 100
    #***     nx <- length(res$x0)
    #***     res$breaks <- c(res$x0, tail(res$x1, n))
    #***     col <- c(head(res$col0, -1), tail(res$col1, n))
    #***     if (n == 1 && 0 <= blend && blend <= 1) {
    #***         for (i in 1:nx) {
    #***             b <- colorRamp(c(res$col0[i], res$col1[i]))(blend)
    #***             col[i] <- rgb(b[1], b[2], b[3], maxColorValue=255)
    #***             ##oceDebug(debug, "blending at i=", i, "\n")
    #***             ##oceDebug(debug, "i=", i, "col", col[i], "col0", res$col0[i], "col1", res$col1[i], "\n")
    #***         }
    #***     }
    #***     res$col <- col
    #***     if (is.null(res$missingColor))
    #***         res$missingColor <- "gray"
    #***     if (zKnown) {
    #***         ## BOOKMARK2 -- this code needs to be in synch with BOOKMARK1
    #***         oceDebug(debug, "z is known ... determining zcol now\n")
    #***         i <- findInterval(z, res$breaks)
    #***         missing <- i == 0
    #***         i[missing] <- 1            # just pick something; replaced later
    #***         res$zcol <- col[i]
    #***         zcol <- col[i]
    #***         res$zcol[missing] <- res$missingColor
    #***         if (zclip) {
    #***             res$zcol[missing] <- res$missingColor
    #***         } else {
    #***             if (zKnown)
    #***                 res$zcol[is.na(z)] <- res$missingColor
    #***             res$zcol[z <= min(res$breaks)] <- col[1]
    #***             res$zcol[z >= max(res$breaks)] <- tail(col, 1)
    #***         }
    #***     } else {
    #***         oceDebug(debug, 'z is missing, so zcol="black"\n')
    #***         res$zcol <- "black"
    #***     }
    #*** }
    res$missingColor <- if (missingColorKnown) missingColor else "gray"
    res$zclip <- zclip
    res$colfunction <- function(z) res$col0[findInterval(z, res$x0)]
    class(res) <- c("list", "colormap")
    oceDebug(debug, "} # colormap()\n", style="bold", sep="", unindent=1)
    res
}

## keeping this (which was called 'colormap' until 2014-05-07), but not in NAMESPACE.
colormap_colormap <- function(name, x0, x1, col0, col1, n=1, zclip=FALSE, debug=getOption("oceDebug"))
{
    nameKnown <- !missing(name)
    oceDebug(debug, "colormap_colormap(name=",
             if (nameKnown) paste0("\"", name, "\"") else "(MISSING)",
             ", ...) {\n", sep="", unindent=1)
    if (nameKnown) {
        oceDebug(debug, "name was specified, so using colormapGMT() to compute colormap\n")
        res <- colormapGMT(name, debug=debug-1)
    } else {
        oceDebug(debug, "name was not specified, so using x0, x1, etc., to compute colormap\n")
        if (missing(x0) || missing(x1) || missing(col0) || missing(col1))
            stop('give either "name" or all of: "x0", "x1", "col0" and "col1"')
        xlen <- length(x0)
        if (length(x1) != xlen)
            stop('lengths of "x0" and "x1" must agree')
        if (length(col0) != xlen)
            stop('lengths of "x0" and "col0" must agree')
        if (length(col1) != xlen)
            stop('lengths of "x0" and "col1" must agree')
        x0r <- x1r <- col0r <- col1r <- NULL
        if (length(n) != xlen - 1)
            n <- rep(n[1], length.out=xlen)
        oceDebug(debug, "x0:", x0, "\n")
        oceDebug(debug, "x1:", x1, "\n")
        oceDebug(debug, "col0:", col0, "\n")
        oceDebug(debug, "col1:", col1, "\n")
        ##x0A <- c(x0, tail(x1, 1))
        for (i in 2:xlen) {
            dx0 <- (x0[i] - x0[i-1]) / n[i-1]
            x0r <- c(x0r, seq(x0[i-1], by=dx0, length.out=n[i-1]))
            dx1 <- (x1[i] - x1[i-1]) / n[i-1]
            x1r <- c(x1r, seq(x1[i-1], by=dx1, length.out=n[i-1]))
            col0r <- c(col0r, colorRampPalette(col0[seq.int(i-1, i)])(n[i-1]))
            col1r <- c(col1r, colorRampPalette(col1[seq.int(i-1, i)])(n[i-1]))
            oceDebug(debug, "i=", i, "\n")
            oceDebug(debug, "  x0[i-1]", x0[i-1], "x0[i]", x0[i], "\n")
            oceDebug(debug, "  concat x0:", seq(x0[i-1], by=dx0, length.out=1+n[i-1]), "\n")
            oceDebug(debug, "  col0[i-1]:", col0[i-1], "col0[i]:", col0[i], "\n")
            oceDebug(debug, "  col1[i-1]:", col1[i-1], "col1[i]:", col1[i], "\n")
        }
        ## next is wrong -- should not just tack on one value unless n=1
        x0r <- c(x0r, tail(x0, 1))
        x1r <- c(x1r, tail(x1, 1))
        col0r <- c(col0r, tail(col0, 1))
        col1r <- c(col1r, tail(col1, 1))
        res <- list(x0=x0r, x1=x1r, col0=col0r, col1=col1r)
    }
    res$zclip <- zclip
    class(res) <- c("list", "colormap")
    oceDebug(debug, "} # colormap_colormap()\n", sep="", unindent=1)
    res
}

## internal function for palettes
palette2breakscolor <- function(name,
                                breaksPerLevel=1,
                                topoRegion=c("water", "land", "both"))
{
    knownPalettes <- c("GMT_relief", "GMT_ocean", "globe")
    palette <- pmatch(name, knownPalettes)
    if (is.na(palette))
       stop("unknown palette name \"", name, "\"")
    name <- knownPalettes[palette]
    if (name == "GMT_relief") {
        ## GMT based on
        ## GMT_relief.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
        d <- list(l=1000*c(-8, -7, -6, -5, -4, -3, -2, -1, 0, 0.5, 1, 2, 3, 4, 5, 6, 7),
                  lr=c(0, 0, 0, 0, 0, 86, 172, 211, 70, 120, 146, 198, 250, 250, 252, 252, 253),
                  lg=c(0, 5, 10, 80, 150, 197, 245, 250, 120, 100, 126, 178, 230, 234, 238, 243, 249),
                  lb=c(0, 25, 50, 125, 200, 184, 168, 211, 50, 50, 60, 80, 100, 126, 152, 177, 216),
                  u=1000*c(-7, -6, -5, -4, -3, -2, -1, 0, 0.5, 1, 2, 3, 4, 5, 6, 7, 8),
                  ur=c(0, 0, 0, 0, 86, 172, 211, 250, 120, 146, 198, 250, 250, 252, 252, 253, 255),
                  ug=c(5, 10, 80, 150, 197, 245, 250, 255, 100, 126, 178, 230, 234, 238, 243, 249, 255),
                  ub=c(25, 50, 125, 200, 184, 168, 211, 255, 50, 60, 80, 100, 126, 152, 177, 216, 255),
                  f="#FFFFFF",
                  b="#000000",
                  n="#FFFFFF")
    } else if (name == "GMT_ocean") {
        d <- list(l=1000*c(-8, -7, -6, -5, -4, -3, -2, -1),
                  lr=c(0, 0, 0, 0, 0, 86, 172, 211),
                  lg=c(0, 5, 10, 80, 150, 197, 245, 250),
                  lb=c(0, 25, 50, 125, 200, 184, 168, 211),
                  u=1000*c(-7, -6, -5, -4, -3, -2, -1, 0),
                  ur=c(0, 0, 0, 0, 0, 86, 172, 211, 250),
                  ug=c(5, 10, 80, 150, 197, 245, 250, 255),
                  ub=c(25, 50, 125, 200, 184, 168, 211, 255),
                  f="#FFFFFF",
                  b="#000000",
                  n="#FFFFFF")
    } else if (name == "globe") {
        d <- list(l=1000*c(-10, -9.5, -9, -8.5, -8, -7.5, -7, -6.5, -6, -5.5, -5, -4.5, -4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, -0.2, 0, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5),
                  lr=c(153, 153, 153, 136, 119, 102, 85, 68, 51, 34, 17, 0, 27, 54, 81, 108, 134, 161, 188, 215, 241, 51, 51, 187, 255, 243, 230, 217, 168, 164, 162, 159, 156, 153, 162, 178, 183, 194, 204, 229, 242, 255, 255),
                  lg=c(0, 0, 0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 164, 175, 186, 197, 208, 219, 230, 241, 252, 102, 204, 228, 220, 202, 184, 166, 154, 144, 134, 123, 113, 102, 89, 118, 147, 176, 204, 229, 242, 255, 255),
                  lb=c(255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 0, 102, 146, 185, 137, 88, 39, 31, 25, 19, 13, 7, 0, 89, 118, 147, 176, 204, 229, 242, 255, 255),
                  u=1000*c(-9.5, -9, -8.5, -8, -7.5, -7, -6.5, -6, -5.5, -5, -4.5, -4, -3.5, -3, -2.5, -2, -1.5, -1, -0.5, -0.2, 0, 0.1, 0.2, 0.5, 1, 1.5, 2, 2.5, 3, 3.5, 4, 4.5, 5, 5.5, 6, 6.5, 7, 7.5, 8, 8.5, 9, 9.5, 10),
                  ur=c(153, 153, 153, 136, 119, 102, 85, 68, 51, 34, 17, 0, 27, 54, 81, 108, 134, 161, 188, 215, 241, 51, 187, 255, 243, 230, 217, 168, 164, 162, 159, 156, 153, 162, 178, 183, 194, 204, 229, 242, 255, 255, 255),
                  ug=c(0, 0, 0, 17, 34, 51, 68, 85, 102, 119, 136, 153, 164, 175, 186, 197, 208, 219, 230, 241, 252, 204, 228, 220, 202, 184, 166, 154, 144, 134, 123, 113, 102, 89, 118, 147, 176, 204, 229, 242, 255, 255, 255),
                  ub=c(255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 102, 146, 185, 137, 88, 39, 31, 25, 19, 13, 7, 0, 89, 118, 147, 176, 204, 229, 242, 255, 255, 255),
                  f="#FFFFFF",
                  b="#000000",
                  n="#808080")
    } else {
        stop("'", palette, "' is not a recognized value for 'palette'")
    }
    nlevel <- length(d$l)
    breaks <- NULL
    col <- NULL
    for (l in 1:nlevel) {
        lowerColor <- rgb(d$lr[l]/255, d$lg[l]/255, d$lb[l]/255)
        upperColor <- rgb(d$ur[l]/255, d$ug[l]/255, d$ub[l]/255)
        breaks <- c(breaks, seq(d$l[l], d$u[l], length.out=1+breaksPerLevel))
        col <- c(col, colorRampPalette(c(lowerColor, upperColor))(1+breaksPerLevel))
    }
    if (palette %in% c("GMT_relief")) {
        if (topoRegion == "water") {
            wet <- breaks <= 0
            breaks <- breaks[wet]
            col <- col[wet]
        } else if (topoRegion == "land") {
            dry <- breaks >= 0
            breaks <- breaks[dry]
            col <- col[dry]
        }
    }
    ## remove last color since must have 1 more break than color
    col <- head(col, -1)
    list(breaks=breaks, col=col, f=d$f, b=d$b, n=d$n)
}

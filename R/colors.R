## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:

colormapNames <- c("gmt_relief", "gmt_ocean", "gmt_globe", "gmt_gebco")

colorize <- function(z, breaks, col=oceColorsJet, colormap, segments=1, missingColor="gray")
{
    if (missing(colormap)) {
        if (is.function(col)) {
            if (missing(breaks)) {
                breaks <- pretty(z, n=10)
            }
            if (length(breaks) == 1)
                breaks <- pretty(z, n=breaks)
            col <- col(length(breaks) - 1)
        } else {
            ## FIMXE: should perhaps check it's a colour
            col <- col
        }
        ## FIXME: next might miss top colour
        if (missing(z)) {
            zlim <- range(breaks)
            zcol <- "black"
        } else {
            zlim <- range(z, na.rm=TRUE)
            zcol <- col[findInterval(z, breaks)]
        }
    } else {
        if (!missing(col))
            stop("cannot supply 'col' and 'colormap' at the same time")
        if (!missing(breaks))
            stop("cannot supply 'breaks' and 'colormap' at the same time")
        if (is.character(colormap)) {
            colormap <- colormap(colormap)
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
        ## FIXME: next might miss top colour
        if (missing(z)) {
            zlim <- range(breaks)
            zcol <- "black"
        } else {
            zlim <- range(z, na.rm=TRUE)
            zcol <- col[findInterval(z, breaks)]
        }
    }
    list(zlim=zlim, breaks=breaks, col=col, zcol=zcol, missingColor=missingColor)
}

colormapGMT <- function(x0, x1, col0, col1, bpl=1)
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
    rval <- list(breaks=breaks, col=col)
    rval
}

colormapFromGmt <- function(file)
{
    if (missing(file) && missing(text))
        stop("must give either 'file' or 'text'\n")
    if (missing(file)) {
        text <- strsplit(text, '\\n')[[1]]
    } else {
        text <- readLines(file)
    }
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
            missingColor <- if (length(grep("[a-zA-Z]", N))) N else gray(as.numeric(N) / 255)
        } else if (3 == Nlen) {
            missingColor <- rgb(N[1], N[2], N[3], maxColorValue=255)
        } else {
            warning("cannot decode missingColor from \"", line, "\"")
        }
    }
    rval <- list(x0=d$x0, x1=d$x1, col0=col0, col1=col1, missingColor=N)
    class(rval) <- c("list", "colormap")
    rval
}

colormapFromName <- function(name)
{
    id <- pmatch(name, colormapNames)
    if (is.na(id))
        stop("unknown colormap name \"", name, "\"; try one of: ", paste(names, collapse=", "))
    name <- colormapNames[id]
    if (name == "gmt_relief") {
        ##	$Id: GMT_relief.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
        ##
        ## Colortable for whole earth relief used in Wessel topomaps
        ## Designed by P. Wessel and F. Martinez, SOEST
        ## COLOR_MODEL = RGB
        text <- "
        -8000	0	0	0	-7000	0	5	25
        -7000	0	5	25	-6000	0	10	50
        -6000	0	10	50	-5000	0	80	125
        -5000	0	80	125	-4000	0	150	200
        -4000	0	150	200	-3000	86	197	184
        -3000	86	197	184	-2000	172	245	168
        -2000	172	245	168	-1000	211	250	211
        -1000	211	250	211	0	250	255	255
        0	70	120	50	500	120	100	50
        500	120	100	50	1000	146	126	60
        1000	146	126	60	2000	198	178	80
        2000	198	178	80	3000	250	230	100
        3000	250	230	100	4000	250	234	126
        4000	250	234	126	5000	252	238	152
        5000	252	238	152	6000	252	243	177
        6000	252	243	177	7000	253	249	216
        7000	253	249	216	8000	255	255	255
        F	255	255	255				
        B	0	0	0
        N	255	255	255"
    } else if (name == "gmt_ocean") {
        ##	$Id: GMT_ocean.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
        ##
        ## Colortable for oceanic areas as used in Wessel maps
        ## Designed by P. Wessel and F. Martinez, SOEST.
        ## COLOR_MODEL = RGB
        text <- "
        -8000	0	0	0	-7000	0	5	25
        -7000	0	5	25	-6000	0	10	50
        -6000	0	10	50	-5000	0	80	125
        -5000	0	80	125	-4000	0	150	200
        -4000	0	150	200	-3000	86	197	184
        -3000	86	197	184	-2000	172	245	168
        -2000	172	245	168	-1000	211	250	211
        -1000	211	250	211	0	250	255	255
        F	255	255	255
        B	0	0	0"
    } else if (name == "gmt_globe") {
        ##       $Id: GMT_globe.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
        ##
        ## Colormap using in global relief maps
        ## Bathymetry colours manually redefined for blue-shade effect and
        ## new topography colour scheme for use with Generic Mapping Tools.
        ## Designed by Lester M. Anderson (CASP, UK) lester.anderson@casp.cam.ac.uk
        ## COLOR_MODEL = RGB
        text <- "
        -10000	153	0	255	-9500	153	0	255
        -9500	153	0	255	-9000	153	0	255
        -9000	153	0	255	-8500	153	0	255
        -8500	136	17	255	-8000	136	17	255  
        -8000	119	34	255	-7500	119	34	255
        -7500	102	51	255	-7000	102	51	255
        -7000	85	68	255	-6500	85	68	255
        -6500	68	85	255	-6000	68	85	255
        -6000	51	102	255	-5500	51	102	255
        -5500	34	119	255	-5000	34	119	255  
        -5000	17	136	255	-4500	17	136	255
        -4500	0	153	255	-4000	0	153	255
        -4000	27	164	255	-3500	27	164	255
        -3500	54	175	255	-3000	54	175	255
        -3000	81	186	255	-2500	81	186	255
        -2500	108	197	255	-2000	108	197	255
        -2000	134	208	255	-1500	134	208	255
        -1500	161	219	255	-1000	161	219	255
        -1000	188	230	255	-500	188	230	255
        -500	215	241	255	-200	215	241	255
        -200	241	252	255	0	241	252	255
        0	51	102	0	100	51	204	102
        100	51	204	102	200	187	228	146
        200	187	228	146	500	255	220	185
        500	255	220	185	1000	243	202	137
        1000	243	202	137	1500	230	184	88
        1500	230	184	88	2000	217	166	39
        2000	217	166	39	2500	168	154	31
        2500	168	154	31	3000	164	144	25
        3000	164	144	25	3500	162	134	19
        3500	162	134	19	4000	159	123	13
        4000	159	123	13	4500	156	113	7
        4500	156	113	7	5000	153	102	0
        5000	153	102	0	5500	162	89	89
        5500	162	89	89	6000	178	118	118
        6000	178	118	118	6500	183	147	147
        6500	183	147	147	7000	194	176	176
        7000	194	176	176	7500	204	204	204
        7500	204	204	204	8000	229	229	229
        8000	229	229	229	8500	242	242	242
        8500	242	242	242	9000	255	255	255
        9000	255	255	255	9500	255	255	255
        9500	255	255	255	10000	255	255	255
        F	255	255	255				
        B	0	0	0
        N	128	128	128"
    } else if (name == "gmt_gebco") {
        ##	$Id: GMT_gebco.cpt,v 1.1.1.1 2000/12/28 01:23:45 gmt Exp $
        ##
        ## Bathymetry colors approximating the GEBCO charts
        ## Designed by Andrew Goodwillie, Scripps
        ## COLOR_MODEL = RGB
        text <- "
        -7000   0       240     255     -6000   0       240     255
        -6000   35      255     255     -5000   35      255     255
        -5000   90      255     255     -4000   90      255     255
        -4000   140     255     230     -3000   140     255     230
        -3000   165     255     215     -2000   165     255     215
        -2000   195     255     215     -1000   195     255     215
        -1000   210     255     215     -500    210     255     215
        -500    230     255     240     -200    230     255     240
        -200    235     255     255     -0      235     255     255
        F	255	255	255				
        B	0	0	0
        N	128	128	128"
    } else {
        stop("unknown colormap name; try one of: ", paste(colormapNames, collapse=", "))
    }
    colormapFromGmt(textConnection(text))
}

Colormap <- function(z,
                     breaks, col=oceColorsJet,
                     name, x0, x1, col0, col1, n=1,
                     missingColor,
                     debug=getOption("oceDebug"))
{
    oceDebug(debug, "colormap() {\n", unindent=1)
    zKnown <- !missing(z)
    breaksKnown <- !missing(breaks)
    nameKnown <- !missing(name)
    missingColorKnown <- !missing(missingColor)
    xcolKnown <- !missing(x0) && !missing(x1) && !missing(col0) && !missing(col1)
    n <- as.integer(floor(0.5 + n))
    if (zKnown && !breaksKnown && !nameKnown && !xcolKnown) {
        oceDebug(debug, "processing case A\n")
        breaks <- pretty(z, n=10)
        breaksKnown <- TRUE            # trick following code
    }
    if (breaksKnown) {
        oceDebug(debug, "processing case B\n")
        if (n > 1L) {
            warning('n is ignored for the breaks+col method')
            ## FIXME: check if this is better than other code
            ## nbreaks <- length(breaks)
            ## breaks2 <- NULL
            ## for (bi in 2:nbreaks) {
            ##     delta <- (breaks[bi] - breaks[bi-1]) / n
            ##     breaks2 <- c(breaks2, seq(from=breaks[bi-1], by=delta, length.out=n))
            ## }
            ## breaks <- breaks2
            ## if (!is.function(col)) {
            ##     ncol <- length(col)
            ##     col2 <- NULL
            ##     for (ci in 2:ncol) {
            ##         col2 <- c(col2, colorRampPalette(c(col[bi-1], col[bi]))(1+n))
            ##     }
            ##     col <- col2
            ## }
        }
        if (zKnown) {
            rval <- colorize(z=z, breaks=breaks, col=col)
        } else {
            if (length(breaks) < 2)
                stop('must supply "z" if length(breaks)==1')
            rval <- colorize(breaks=breaks, col=col)
            rval$zcol <- "black"
        }
        ## must add x0, x1, col0, col1
        rval$x0 <- rval$breaks[-1] # FIXME: not sure on which to drop
        rval$x1 <- rval$breaks[-1] # FIXME: not sure on which to drop
        rval$col0 <- rval$col
        rval$col1 <- rval$col
    } else {
        if (nameKnown) {
            oceDebug(debug, "processing case C\n")
            rval <- colormap(name)
        } else {
            if (xcolKnown) {
                oceDebug(debug, "processing case D\n")
                rval <- colormap(x0=x0, x1=x1, col0=col0, col1=col1, n=n)
            } else {
                breaks <- pretty(z)
                stop('must give "breaks", "name", or each of "x0", "x1", "col0", and "col1"')
            }
        }
        ## issue 435 work below
        rval$zlim <- if (zKnown) range(z) else range(c(rval$x0, rval$x1))
        nx0 <- length(rval$x0)
        eps <- diff(rval$x0[1:2]) / 100
        rval$breaks <- c(rval$x0[1]-eps, rval$x0, rval$x0[nx0] + eps)
        ##rval$col <- rval$col0 # misses last col1
        rval$col <- c(rval$col0, tail(rval$col1,1))
        rval$zlim <- 1.04*(if (zKnown) range(z) else range(c(rval$x0, rval$x1)))
        rval$zcol <- if (zKnown) col[findInterval(z, rval$breaks)] else "black"
    }
    oceDebug(debug, "} # colormap()\n", unindent=1)
    rval
}

colormap <- function(name, x0, x1, col0, col1, n=1)
{
    if (missing(name)) {
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
        ##cat("n:", n, "\n")
        for (i in 2:xlen) {
            dx0 <- (x0[i] - x0[i-1]) / n[i-1]
            x0r <- c(x0r, seq(x0[i-1], by=dx0, length.out=1+n[i-1]))
            dx1 <- (x1[i] - x1[i-1]) / n[i-1]
            x1r <- c(x1r, seq(x1[i-1], by=dx1, length.out=1+n[i-1]))
            col0r <- c(col0r, colorRampPalette(col0[seq.int(i-1,i)])(1+n[i-1]))
            col1r <- c(col1r, colorRampPalette(col1[seq.int(i-1,i)])(1+n[i-1]))
            ## cat("i=", i,
            ##     "\n\tx0[i-1]", round(x0[i-1]), "x0[i]", round(x0[i]),
            ##     "\n\tconcat x0:",seq(x0[i-1], by=dx0, length.out=1+n[i-1]),
            ##     "\n\tcol0[i-1]:", col0[i-1], "col0[i]:", col0[i],
            ##     "\n\tcol1[i-1]:", col1[i-1], "col1[i]:", col1[i],
            ##     "\n") 
        }
        rval <- list(x0=x0r, x1=x1r, col0=col0r, col1=col1r)
        class(rval) <- c("list", "colormap")
    } else {
        id <- pmatch(name, colormapNames)
        ## NB> next two functions not in NAMESPACE
        rval <- if (is.na(id)) colormapFromGmt(name) else
            colormapFromName(colormapNames[id])
    }
    rval
}

colormapOLD <- function(name, file, breaks, col, type=c("level", "gradient"), mcol="gray", fcol="white")
{
    if (!missing(name)) { # takes precedence over all
        nameList <- c("gmt_relief", "gmt_ocean", "gmt_globe")
        n <- pmatch(name, nameList)
        if (is.na(n))
            stop("unknown 'name'; must be one of: ", paste(nameList, collapse=" "))
        name <- nameList[n]
        stop("should handle name '", name, "' now")
        return(NULL)
    } else if (!missing(file)) {
        ## colormap('http://www.beamreach.org/maps/gmt/share/cpt/GMT_globe.cpt')
        return(colormapFromGmt(file))
    } else {                           # use lower, upper, and color
        if (missing(breaks) || missing(col)) {
            stop("provide 'breaks' and 'color' if both 'name' and 'file' are missing")
        }
        ## emulate how GMT handled
        breaksPerLevel <- 3            # FIXME
        nbreaks <- length(breaks)
        ncol <- length(col)
        delta <- mean(diff(breaks))
        upper <- c(head(breaks, -1), breaks[nbreaks]+delta) + delta / 2
        lower <- c(breaks[1]-delta, head(breaks,-1)) + delta / 2
        lowerColor <- c(col[1], col)
        upperColor <- c(col, col[ncol])
        breaks2 <- NULL
        col2 <- NULL
        cat("breaks:", breaks, "\n")
        cat("lower:", lower, "\n")
        cat("upper:", upper, "\n")
        for (l in seq.int(1, nbreaks)) {
            cat("l:", l, ", lower[l]:", lower[l], ", upper[l]:", upper[l], "\n")
            breaks2 <- c(breaks2, seq(lower[l], upper[l], length.out=1+breaksPerLevel))
            col2 <- c(col2, colorRampPalette(c(lowerColor[l], upperColor[l]))(1+breaksPerLevel))
        }
        return(list(breaks=breaks2, col=head(col2, -1), mcol=mcol, fcol=fcol, l=breaks2, u=breaks2))# l and u wrong
        if (F) {
            ## centre and extend so get all colours
            nbreaks <- length(breaks)
            l <- c(breaks[1] - (breaks[2] - breaks[1]), breaks)
            u <- c(breaks, breaks[nbreaks] + (breaks[nbreaks] - breaks[nbreaks-1]))
            nb <- nbreaks
            x <- seq(0, 1, length.out=nbreaks+1)
            xout <- seq(0, 1, length.out=nb)
            l <- approx(x, l, xout)$y
            u <- approx(x, u, xout)$y
            col <- colorRampPalette(col)(nb-1)
            list(breaks=seq(min(l), max(u), length.out=nb), col=col, mcol=mcol, fcol=fcol, l=l, u=u)
        }
    }
}


makePalette <- function(style=c("gmt_relief", "gmt_ocean", "oce_shelf"),
                        file, breaksPerLevel=20,
                        region=c("water", "land", "both"))
{
    style <- match.arg(style)
    region <- match.arg(region)
    if (!missing(file)) {
        d <- colormapFromGmt(file)
    } else {
        if (style == "gmt_relief") {
            text <- "
#	$Id: GMT_relief.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
#
# Colortable for whole earth relief used in Wessel topomaps
# Designed by P. Wessel and F. Martinez, SOEST
# COLOR_MODEL = RGB
-8000	0	0	0	-7000	0	5	25
-7000	0	5	25	-6000	0	10	50
-6000	0	10	50	-5000	0	80	125
-5000	0	80	125	-4000	0	150	200
-4000	0	150	200	-3000	86	197	184
-3000	86	197	184	-2000	172	245	168
-2000	172	245	168	-1000	211	250	211
-1000	211	250	211	0	250	255	255
0	70	120	50	500	120	100	50
500	120	100	50	1000	146	126	60
1000	146	126	60	2000	198	178	80
2000	198	178	80	3000	250	230	100
3000	250	230	100	4000	250	234	126
4000	250	234	126	5000	252	238	152
5000	252	238	152	6000	252	243	177
6000	252	243	177	7000	253	249	216
7000	253	249	216	8000	255	255	255
F	255	255	255				
B	0	0	0
N	255	255	255"
        } else if (style == "gmt_ocean") {
            text <- "
#	$Id: GMT_ocean.cpt,v 1.1 2001/09/23 23:11:20 pwessel Exp $
#
# Colortable for oceanic areas as used in Wessel maps
# Designed by P. Wessel and F. Martinez, SOEST.
# COLOR_MODEL = RGB
-8000	0	0	0	-7000	0	5	25
-7000	0	5	25	-6000	0	10	50
-6000	0	10	50	-5000	0	80	125
-5000	0	80	125	-4000	0	150	200
-4000	0	150	200	-3000	86	197	184
-3000	86	197	184	-2000	172	245	168
-2000	172	245	168	-1000	211	250	211
-1000	211	250	211	0	250	255	255
F	255	255	255
B	0	0	0"
        } else if (style == "oce_shelf") {
            text <- "
-500	0	0	0	-200	0	10	55
-200	0	10	55	-175	0	40	80
-175	0	40	80	-150	0	80	125
-150	0	80	125	-125	0	115     162	
-125	0	150	200	-100	43	173     192	
-100	86	197	184	-75	129     221     176
-75	172     245     168	-50	191     247     189
-50	211     250     211	-25     220     250     240	
-25	220	250	240	0	250	255	255"
        } else {
            stop("unknown colormap style \"", style, "\"")
        }
        d <- colormapFromGmt(textConnection(text))
    }
    nlevel <- length(d$x0)
    breaks <- NULL
    col <- NULL
    str(d)
    for (l in 1:nlevel) {
        breaks <- c(breaks, seq(d$x0[l], d$x1[l], length.out=1+breaksPerLevel))
        col <- c(col, colorRampPalette(c(d$col0[l], d$col1[l]))(1+breaksPerLevel))
    }
    if (region == "water") {
        wet <- breaks <= 0
        breaks <- breaks[wet]
        col <- col[wet]
    } else if (region == "land") {
        dry <- breaks >= 0
        breaks <- breaks[dry]
        col <- col[dry]
    }
    ## drop a colour for length match with breaks
    col <- col[-1]                     
    list(breaks=breaks, col=col, f=d$f, b=d$b, n=d$n)
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
        d <- list(l=1000*c(-8,-7,-6,-5,-4,-3,-2,-1,0,0.5,1,2,3,4,5,6,7),
                  lr=c(0,0,0,0,0,86,172,211,70,120,146,198,250,250,252,252,253),
                  lg=c(0,5,10,80,150,197,245,250,120,100,126,178,230,234,238,243,249),
                  lb=c(0,25,50,125,200,184,168,211,50,50,60,80,100,126,152,177,216),
                  u=1000*c(-7,-6,-5,-4,-3,-2,-1,0,0.5,1,2,3,4,5,6,7,8),
                  ur=c(0,0,0,0,86,172,211,250,120,146,198,250,250,252,252,253,255),
                  ug=c(5,10,80,150,197,245,250,255,100,126,178,230,234,238,243,249,255),
                  ub=c(25,50,125,200,184,168,211,255,50,60,80,100,126,152,177,216,255),
                  f="#FFFFFF",
                  b="#000000",
                  n="#FFFFFF")
    } else if (name == "GMT_ocean") {
        d <- list(l=1000*c(-8,-7,-6,-5,-4,-3,-2,-1),
                  lr=c(0,0,0,0,0,86,172,211),
                  lg=c(0,5,10,80,150,197,245,250),
                  lb=c(0,25,50,125,200,184,168,211),
                  u=1000*c(-7,-6,-5,-4,-3,-2,-1,0),
                  ur=c(0,0,0,0,0,86,172,211,250),
                  ug=c(5,10,80,150,197,245,250,255),
                  ub=c(25,50,125,200,184,168,211,255),
                  f="#FFFFFF",
                  b="#000000",
                  n="#FFFFFF")
    } else if (name == "globe") {
        d <- list(l=1000*c(-10,-9.5,-9,-8.5,-8,-7.5,-7,-6.5,-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,-0.2,0,0.1,0.2,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5),
                  lr=c(153,153,153,136,119,102,85,68,51,34,17,0,27,54,81,108,134,161,188,215,241,51,51,187,255,243,230,217,168,164,162,159,156,153,162,178,183,194,204,229,242,255,255),
                  lg=c(0,0,0,17,34,51,68,85,102,119,136,153,164,175,186,197,208,219,230,241,252,102,204,228,220,202,184,166,154,144,134,123,113,102,89,118,147,176,204,229,242,255,255),
                  lb=c(255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,0,102,146,185,137,88,39,31,25,19,13,7,0,89,118,147,176,204,229,242,255,255),
                  u=1000*c(-9.5,-9,-8.5,-8,-7.5,-7,-6.5,-6,-5.5,-5,-4.5,-4,-3.5,-3,-2.5,-2,-1.5,-1,-0.5,-0.2,0,0.1,0.2,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,7.5,8,8.5,9,9.5,10),
                  ur=c(153,153,153,136,119,102,85,68,51,34,17,0,27,54,81,108,134,161,188,215,241,51,187,255,243,230,217,168,164,162,159,156,153,162,178,183,194,204,229,242,255,255,255),
                  ug=c(0,0,0,17,34,51,68,85,102,119,136,153,164,175,186,197,208,219,230,241,252,204,228,220,202,184,166,154,144,134,123,113,102,89,118,147,176,204,229,242,255,255,255),
                  ub=c(255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,102,146,185,137,88,39,31,25,19,13,7,0,89,118,147,176,204,229,242,255,255,255),
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
    ## remove last colour since must have 1 more break than color
    col <- head(col, -1)
    list(breaks=breaks, col=col, f=d$f, b=d$b, n=d$n)
}


resizable.label <- function(item=c("S", "T", "p", "z"), axis=c("x", "y"))
{
    item <- match.arg(item)
    axis <- match.arg(axis)
    if (item == "T") {
        full <- expression(paste("Temperature [ ", degree, "C ]"))
        abbreviated <- expression(paste("T [ ", degree, "C ]"))
    } else if (item == "S") {
        full <- "Salinity [ PSU ]"
        abbreviated <- "S [ PSU ]"
    } else if (item == "p") {
        full <- "Pressure [ dbar ]"
        abbreviated <- "P [ dbar ]"
    } else if (item == "z") {
        full <- "z [ m ]"
        abbreviated <- "z [ m ]"
    }
    fraction <- strwidth(full, "inches") / par("pin")[if(axis == "x") 1 else 2]
    if (fraction < 0.8) full else abbreviated
}

latlon.format <- function(lat, lon, digits=max(6, getOption("digits") - 1))
{
    n <- length(lon)
    rval <- vector("character", n)
    if (!is.numeric(lat) || !is.numeric(lon))
        return ("(non-numeric lat or lon)")
    for (i in 1:n) {
        if (is.na(lat[i]) || is.na(lon[i]))
            rval[i] <- ""
        else
            rval[i] <- paste(format(abs(lat[i]), digits=digits),
                             if (lat[i] > 0) "N  " else "S  ",
                             format(abs(lon[i]), digits=digits),
                             if (lon[i] > 0) "E" else "W",
                             sep="")
    }
    rval
}

lat.format <- function(lat, digits=max(6, getOption("digits") - 1))
{
    n <- length(lat)
    if (n < 1) return("")
    rval <- vector("character", n)
    for (i in 1:n)
        if (is.na(lat[i]))
            rval[i] <-  ""
        else
            rval[i] <- paste(format(abs(lat[i]), digits=digits),
                             if (lat[i] > 0) "N" else "S",
                             sep="")
    rval
}

lon.format <- function(lon, digits=max(6, getOption("digits") - 1))
{
    n <- length(lon)
    if (n < 1) return("")
    rval <- vector("character", n)
    for (i in 1:n)
        if (is.na(lon[i]))
            rval[i] <-  ""
        else
            rval[i] <- paste(format(abs(lon[i]), digits=digits),
                             if (lon[i] > 0) "E" else "S",
                             sep="")
    rval
}

GMT.offset.from.tz <- function(tz)
{
    ## Data are from
    ##   http://www.timeanddate.com/library/abbreviations/timezones/
    ## and hand-edited, so there may be errors.  Also, note that some of these
    ## contradict ... I've commented out conflicting definitions that I think
    ## will come up most rarely in use, but perhaps something better should
    ## be devised.  (Maybe this is not a problem.  Maybe only MEDS uses these,
    ## as opposed to GMT offsets, and maybe they only work in 5 zones, anyway...)
    if (tz == "A"   )  	return( -1  ) # Alpha Time Zone	Military	UTC + 1 hour
    if (tz == "ACDT") 	return(-10.5) # Australian Central Daylight Time   Australia	UTC + 10:30 hours
    if (tz == "ACST")	return( -9.5) # Australian Central Standard Time  Australia	UTC + 9:30 hours
    if (tz == "ADT" )  	return( 3   ) # Atlantic Daylight Time	North America	UTC - 3 hours
    if (tz == "AEDT")	return(-11  ) # Aus. East. Day. Time or Aus. East Summer Time Aus. UTC + 11 hours
    if (tz == "AEST")	return(-10  ) # Australian Eastern Standard Time  Australia UTC + 10 hours
    if (tz == "AKDT")	return(  8  ) # Alaska Daylight Time	North America	UTC - 8 hours
    if (tz == "AKST")	return(  9  ) # Alaska Standard Time	North America	UTC - 9 hours
    if (tz == "AST" )	return(  4  ) # Atlantic Standard Time	North America	UTC - 4 hours
    if (tz == "AWDT") 	return( -9  ) # Australian Western Daylight Time	Australia	UTC + 9 hours
    if (tz == "AWST") 	return( -8  ) # Australian Western Standard Time	Australia	UTC + 8 hours
    if (tz == "B"   )	return( -2  ) # Bravo Time Zone	Military	UTC + 2 hours
    if (tz == "BST" )	return( -1  ) # British Summer Time	Europe	UTC + 1 hour
    if (tz == "C"   )	return( -3  ) # Charlie Time Zone	Military	UTC + 3 hours
    ##if (tz == "CDT")  return(-10.5) # Central Daylight Time	Australia	UTC + 10:30 hours
    if (tz == "CDT" )	return(  5  ) # Central Daylight Time	North America	UTC - 5 hours
    if (tz == "CEDT")	return( -2  ) # Central European Daylight Time	Europe	UTC + 2 hours
    if (tz == "CEST")	return( -2  ) # Central European Summer Time	Europe	UTC + 2 hours
    if (tz == "CET" )	return( -1  ) # Central European Time	Europe	UTC + 1 hour
    ##if (tz == "CST" ) return(-10.5) # Central Summer Time	Australia	UTC + 10:30 hours
    ##if (tz == "CST" ) return( -9.5) # Central Standard Time	Australia	UTC + 9:30 hours
    if (tz == "CST" )	return(  6  ) # Central Standard Time	North America	UTC - 6 hours
    if (tz == "CXT" )	return( -7  ) # Christmas Island Time	Australia	UTC + 7 hours
    if (tz == "D"   )	return( -4  ) # Delta Time Zone	Military	UTC + 4 hours
    if (tz == "E"   )	return( -5  ) # Echo Time Zone	Military	UTC + 5 hours
    ##if (tz == "EDT" ) return( -11 ) # Eastern Daylight Time	Australia	UTC + 11 hours
    if (tz == "EDT" )	return(  4  ) # Eastern Daylight Time	North America	UTC - 4 hours
    if (tz == "EEDT") 	return( -3  ) # Eastern European Daylight Time	Europe	UTC + 3 hours
    if (tz == "EEST") 	return( -3  ) # Eastern European Summer Time	Europe	UTC + 3 hours
    if (tz == "EET")	return( -2  ) # Eastern European Time	Europe	UTC + 2 hours
    ##if (tz == "EST")  return( -11 ) # Eastern Summer Time	Australia	UTC + 11 hours
    ##if (tz == "EST")  return( -10 ) # Eastern Standard Time	Australia	UTC + 10 hours
    if (tz == "EST" )	return(  5  ) # Eastern Standard Time	North America	UTC - 5 hours
    if (tz == "F"   )	return( -6  ) # Foxtrot Time Zone	Military	UTC + 6 hours
    if (tz == "G"   )	return( -7  ) # Golf Time Zone	Military	UTC + 7 hours
    if (tz == "GMT" )	return(  0  ) # Greenwich Mean Time	Europe	UTC
    if (tz == "H"   )	return( -8  ) # Hotel Time Zone	Military	UTC + 8 hours
    if (tz == "HAA" )	return(  3  ) # Heure Avancée de l'Atlantique	North America	UTC - 3 hours
    if (tz == "HAC" )	return(  5  ) # Heure Avancée du Centre	North America	UTC - 5 hours
    if (tz == "HADT")	return(  9  ) # Hawaii-Aleutian Daylight Time	North America	UTC - 9 hours
    if (tz == "HAE" )	return(  4  ) # Heure Avancée de l'Est	North America	UTC - 4 hours
    if (tz == "HAP" )	return(  7  ) # Heure Avancée du Pacifique	North America	UTC - 7 hours
    if (tz == "HAR" )	return(  6  ) # Heure Avancée des Rocheuses	North America	UTC - 6 hours
    if (tz == "HAST")	return( 10  ) # Hawaii-Aleutian Standard Time	North America	UTC - 10 hours
    if (tz == "HAT" )	return(  2.5) # Heure Avancée de Terre-Neuve	North America	UTC - 2:30 hours
    if (tz == "HAY" )	return(  8  ) # Heure Avancée du Yukon	North America	UTC - 8 hours
    if (tz == "HNA" )	return(  4  ) # Heure Normale de l'Atlantique	North America	UTC - 4 hours
    if (tz == "HNC" )	return(  6  ) # Heure Normale du Centre	North America	UTC - 6 hours
    if (tz == "HNE" )	return(  5  ) # Heure Normale de l'Est	North America	UTC - 5 hours
    if (tz == "HNP" )	return(  8  ) # Heure Normale du Pacifique	North America	UTC - 8 hours
    if (tz == "HNR" )	return(  7  ) # Heure Normale des Rocheuses	North America	UTC - 7 hours
    if (tz == "HNT" )	return(  3.5) # Heure Normale de Terre-Neuve	North America	UTC - 3:30 hours
    if (tz == "HNY" )	return(  9  ) # Heure Normale du Yukon	North America	UTC - 9 hours
    if (tz == "I"   )	return( -9  ) # India Time Zone	Military	UTC + 9 hours
    if (tz == "IST" )	return( -1  ) # Irish Summer Time	Europe	UTC + 1 hour
    if (tz == "K"   )	return(-10  ) # Kilo Time Zone	Military	UTC + 10 hours
    if (tz == "L"   )	return(-11  ) # Lima Time Zone	Military	UTC + 11 hours
    if (tz == "M"   )	return(-12  ) # Mike Time Zone	Military	UTC + 12 hours
    if (tz == "MDT" )	return(  6  ) # Mountain Daylight Time	North America	UTC - 6 hours
    if (tz == "MESZ") 	return( -2  ) # Mitteleuroäische Sommerzeit	Europe	UTC + 2 hours
    if (tz == "MEZ" )	return( -1  ) # Mitteleuropäische Zeit	Europe	UTC + 1 hour
    if (tz == "MST" )	return(  7  ) # Mountain Standard Time	North America	UTC - 7 hours
    if (tz == "N"   )	return(  1  ) # November Time Zone	Military	UTC - 1 hour
    if (tz == "NDT" )	return(  2.5) # Newfoundland Daylight Time	North America	UTC - 2:30 hours
    if (tz == "NFT" )	return(-11.5) # Norfolk (Island) Time	Australia	UTC + 11:30 hours
    if (tz == "NST" )	return(  3.5) # Newfoundland Standard Time	North America	UTC - 3:30 hours
    if (tz == "O"   )	return(  1  ) # Oscar Time Zone	Military	UTC - 2 hours
    if (tz == "P"   )	return(  3  ) # Papa Time Zone	Military	UTC - 3 hours
    if (tz == "PDT" )	return(  7  ) # Pacific Daylight Time	North America	UTC - 7 hours
    if (tz == "PST" )	return(  8  ) # Pacific Standard Time	North America	UTC - 8 hours
    if (tz == "Q"   )	return(  4  ) # Quebec Time Zone	Military	UTC - 4 hours
    if (tz == "R"   )	return(  4  ) # Romeo Time Zone	Military	UTC - 5 hours
    if (tz == "S"   )	return(  6  ) # Sierra Time Zone	Military	UTC - 6 hours
    if (tz == "T"   )	return(  7  ) # Tango Time Zone	Military	UTC - 7 hours
    if (tz == "U"   )	return(  8  ) # Uniform Time Zone	Military	UTC - 8 hours
    if (tz == "UTC" )	return(  0  ) # Coordinated Universal Time	Europe	UTC
    if (tz == "V"   )	return(  9  ) # Victor Time Zone	Military	UTC - 9 hours
    if (tz == "W"   )	return( 10  ) # Whiskey Time Zone	Military	UTC - 10 hours
    if (tz == "WDT" )	return( -9  ) # Western Daylight Time	Australia	UTC + 9 hours
    if (tz == "WEDT") 	return( -1  ) # Western European Daylight Time	Europe	UTC + 1 hour
    if (tz == "WEST") 	return( -1  ) # Western European Summer Time	Europe	UTC + 1 hour
    if (tz == "WET")	return(  0  ) # Western European Time	Europe	UTC
    ##if (tz == "WST")  return( -9  ) # Western Summer Time	Australia	UTC + 9 hours
    if (tz == "WST")	return( -8  ) # Western Standard Time	Australia	UTC + 8 hours
    if (tz == "X"  )	return( 11  ) # X-ray Time Zone	Military	UTC - 11 hours
    if (tz == "Y"  )	return( 12  ) # Yankee Time Zone	Military	UTC - 12 hours
    if (tz == "Z"  )	return(  0  ) # Zulu Time Zone	Military	UTC
}

gravity <- function(latitude=45, degrees=TRUE)
{
    if (degrees) latitude <- latitude * 0.0174532925199433
    9.780318*(1.0+5.3024e-3*sin(latitude)^2-5.9e-6*sin(2*latitude)^2)
}

make.filter <- function(type=c("blackman-harris"), m)
{
    type <- match.arg(type)
    if (type == "blackman-harris") {    # See Harris (1978)
        if (missing(m)) stop("must supply 'm'")
        m <- floor(m)
        if (!(m %% 2)) m <- m + 1 # now, m is an odd integer
        n <- seq(0, m - 1)
        a <- c(0.35875, 0.488829, 0.14128, 0.01168)
        ff <- pi * n / (m - 1)
        coef <- a[1] - a[2]*cos(2*ff) + a[3]*cos(4*ff) - a[4]*cos(6*ff)
        coef / sum(coef)                # make unit sum
    }
}

## Calculation of geodetic distance on surface of earth,
## based upon datum defined by
##       a = radius of major axis of earth
##       f = flattening factor.
## The answer is returned in the same units as a; here in meters.
##
## Patterned after R code donated by Darren Gillis
geod.xy <- function(lat, lon, lat.ref, lon.ref, rotate=0)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(lat)) stop("must provide lat")
    if (missing(lon)) stop("must provide lat")
    if (missing(lat.ref)) stop("must provide lat.ref")
    if (missing(lon.ref)) stop("must provide lon.ref")
    if (!is.finite(lat.ref)) stop("lat.ref must be finite")
    if (!is.finite(lon.ref)) stop("lat.ref must be finite")
    n <- length(lat)
    if (length(lon) != n) stop("lat and lon must be vectors of the same length")
    x <- y <- vector("numeric", n)
    xy  <- .C("geod_xy",
              as.integer(n),
              as.double(lat),
              as.double(lon),
              as.double(lat.ref),
              as.double(lon.ref),
              as.double(a),
              as.double(f),
              x = double(n),
              y = double(n),
              PACKAGE = "oce")
    x <- xy$x
    y <- xy$y
    if (rotate != 0) {
        S <- sin(rotate * pi / 180)
        C <- cos(rotate * pi / 180)
        r <- matrix(c(C,S,-S,C),nrow=2)
        rxy <- r %*% rbind(x,y)
        x <- rxy[1,]
        y <- rxy[2,]
    }
    data.frame(x, y)
}

geod.dist <- function (lat1, lon1=NULL, lat2=NULL, lon2=NULL)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (inherits(lat1, "section")) {
        copy <- lat1
        n <- length(copy$data$station)
        lat1 <- vector("numeric", n)
        lon1 <- vector("numeric", n)
        for (i in 1:n) {
            lat1[i] <- copy$data$station[[i]]$metadata$latitude
            lon1[i] <- copy$data$station[[i]]$metadata$longitude
        }
        res <- vector("numeric", n)
        for (i in 1:n) {
            if (is.finite(lat1[1]) && is.finite(lon1[1]) && is.finite(lat1[i]) && is.finite(lon1[i])) {
                ## dist <- .Fortran("geoddist",
                dist <- .C("geoddist",
                           as.double(lat1[1]),
                           as.double(lon1[1]),
                           as.double(lat1[i]),
                           as.double(lon1[i]),
                           as.double(a),
                           as.double(f),
                           as.double(1),
                           as.double(1),
                           dist = double(1),
                           PACKAGE = "oce")$dist
            } else {
                dist <- NA
            }
            res[i] <- dist
        }
    } else {
        n1 <- length(lat1)
        if (length(lon1) != n1)	stop("lat1 and lon1 must be vectors of the same length")
        n2 <- length(lat2)
        if (length(lon2) != n2)	stop("lat2 and lon2 must be vectors of the same length")
        if (n2 < n1) { # take only first one
            if (n2 != 1) warning("Using just the first element of lat2 and lon2, even though it contains more elements")
            llat2 <- rep(lat2[1], n1)
            llon2 <- rep(lon2[1], n1)
        } else {
            llat2 <- lat2
            llon2 <- lon2
        }
                                        #subroutine geoddist(DLAT1,DLON1,DLAT2,DLON2,A,F,FAZ,BAZ,S)
        res <- vector("numeric", n1)
        for (i in 1:n1) {
                                        #cat("values=",lat1[i],lon1[i],llat2[i],llon2[i],"\n")
            if (is.finite(lat1[i]) && is.finite(lon1[i]) && is.finite(llat2[i]) && is.finite(llon2[i])) {
                ## res[i] <- .Fortran("geoddist",
                res[i] <- .C("geoddist",
                             as.double(lat1[i]),
                             as.double(lon1[i]),
                             as.double(llat2[i]),
                             as.double(llon2[i]),
                             as.double(a),
                             as.double(f),
                             as.double(1),
                             as.double(1),
                             dist = double(1),
                             PACKAGE = "oce")$dist
            } else {
                res[i] <- NA
            }
        }
    }
    res / 1000
}

interp.barnes <- function(x, y, z, w=NULL, xg=NULL, yg=NULL,
                          xr=NULL, yr=NULL, gamma=0.5, iterations=2)
{
    n <- length(x)
    if (length(y) != n) stop("lengths of x and y disagree; they are %s and %s", n, length(y))
    if (length(z) != n) stop("lengths of x and z disagree; they are %s and %s", n, length(z))
    if (is.null(w)) {
        w <- rep(1.0, length(x))
        cat("interp.barnes assuming equal weights on all data\n")
    }
    if (is.null(xg)) {
        xg <- pretty(x, n=50)
        cat("interp.barnes using calculated value xg =", xg[1], ",", xg[2], ",...,", xg[length(xg)], "\n")
    }
    if (is.null(yg)) {
        if (0 == diff(range(y))) {
            yg <- y[1]
            cat("interp.barnes using calculated value yg =", yg[1], "\n")
        } else {
            yg <- pretty(y, n=50)
            cat("interp.barnes using calculated value yg =", yg[1], ",", yg[2], ",...,", yg[length(yg)],"\n")
        }
    }
    if (is.null(xr)) {
        xr <- diff(range(x)) / sqrt(n)
        if (xr == 0) xr <- 1
        cat("interp.barnes using calculated value xr =", xr, "\n")
    }
    if (is.null(yr)) {
        yr <- diff(range(y)) / sqrt(n)
        if (yr == 0) yr <- 1
        cat("interp.barnes using calculated value yr =", yr, "\n")
    }
    zg <- .Call("interp_barnes",
                as.double(x),
                as.double(y),
                as.double(z),
                as.double(w),
                as.double(xg),
                as.double(yg),
                as.double(xr),
                as.double(yr),
                as.double(gamma),
                as.integer(iterations))
    list(xg=xg, yg=yg, zg=zg)
}

coriolis <- function(lat, degrees=TRUE)
{
    if (degrees) lat <- lat * 0.0174532925199433
    1.4544410433286078e-4 * sin(lat)
}

undrift.time <- function(x, slow.end = 0, tname="time")
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    names <- names(x$data)
    if (!(tname %in% names)) stop("no column named '", tname, "'; only found: ", paste(names, collapse=" "))
    rval <- x
    time <- rval$data[[tname]]
    nt <- length(time)
    if (nt < 2) warning("too few data to to undrift time; returning object unaltered")
    else {
        sample.interval <- as.numeric(difftime(time[2], time[1], units="s"))
        nt <- length(time)
        nt.out <- floor(0.5 + nt + slow.end / sample.interval)
        time.out <- seq.POSIXt(from=time[1], by=sample.interval, length.out=nt.out)
        i <- seq(from=1, by=1, length.out=nt)
        i.out <- seq(from=1, to=nt, length.out = nt.out)
        out <- data.frame(array(dim=c(nt.out, dim(x$data)[2])))
        names(out) <- names
        out[[tname]] <- time.out
        for (name in names) {
            if (name != tname) {
                yy <- approx(x=i, y=x$data[[name]], xout=i.out)$y
                out[[name]] <- yy
            }
        }
        rval$data <- out
    }
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

fill.gap <- function(x, start, end, column)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (missing(start)) stop("must supply start")
    if (missing(end)) stop("must supply end")
    if (!missing(column)) {
        start <- which(x$data[[column]] == start)[1]
        end <- which(x$data[[column]] == end)[1]
    }
    if (end - start < 1) stop("end must be at least 1+start")
    rval <- x
    i <- start:end
    for (name in names(x$data)) {
        filler <- approx(x=c(start, end), y=x$data[[name]][c(start,end)], xout=i)$y
        class(filler) <- class(x$data[[name]])
        rval$data[[name]][i] <- filler
    }
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

oce.colors.gebco <- function(n=9, region=c("water", "land", "both"), type=c("fill","line"))
{
    region <- match.arg(region)
    type <- match.arg(type)
    if (type == "fill") {
        ## generate land colors by e.g. rgb(t(col2rgb(land[5])-1*c(10,4,10))/255)
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- c("#E1FCF7","#BFF2EC","#A0E8E4","#83DEDE","#68CDD4",
                   "#4FBBC9","#38A7BF","#2292B5","#0F7CAB")
    } else {
        land <- c("#FBC784","#F1C37A","#E6B670","#DCA865","#D19A5C",
                  "#C79652","#BD9248","#B38E3E","#A98A34")
        water <- c("#A4FCE3","#72EFE9","#4FE3ED","#47DCF2","#46D7F6",
                   "#3FC0DF","#3FC0DF","#3BB7D3","#36A5C3","#3194B4",
                   "#2A7CA4","#205081","#16255E","#100C2F")
    }
    if (region == "water") {
        rgb.list <- col2rgb(water) / 255
        l <- length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else if (region == "land") {
        rgb.list <- col2rgb(land) / 255
        l <- length(land)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    } else {                            # both
        rgb.list <- col2rgb(c(land ,water)) / 255
        l <- length(land) + length(water)
        r <- approx(1:l, rgb.list[1,1:l], xout=seq(1, l, length.out=n))$y
        g <- approx(1:l, rgb.list[2,1:l], xout=seq(1, l, length.out=n))$y
        b <- approx(1:l, rgb.list[3,1:l], xout=seq(1, l, length.out=n))$y
    }
    rgb(r, g, b)
}

header <- function(x)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    return(x$metadata$header)
}

add.column <- function (x, data, name)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    if (missing(data)) stop("must supply data")
    if (missing(name)) stop("must supply name")
    n <- dim(x$data)[1]
    nd <- length(data)
    if (nd != n) stop("data length is ", nd, " but it must be ", n, " to match existing data")
    rval <- x
    rval$data <- data.frame(x$data, data)
    names(rval$data) <- c(names(x$data), name)
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

decimate <- function(x, by=10, method=c("direct", "filter"), filter)
{
    if (!inherits(x, "oce")) stop("method is only for oce objects")
    method <- match.arg(method)
    res <- x
    if (method == "direct") {
        i <- seq(1, dim(x$data)[1], by=by)
        res$data <- x$data[i,]
    } else if (method == "filter") {
        if (missing(filter)) stop("must supply a filter")
        nvar <- dim(x$data)[2]
        for (var in 1:nvar) {
            res$data[,var] <- filter(x$data[,var], filter)
            fill <- is.na(res$data[,var])
            res$data[fill,var] <- x$data[fill,var]
            ##if (var==2)print(data.frame(fill=fill,orig=x$data[,var],new=res$data[,var]))
        }
        i <- seq(1, dim(x$data)[1], by=by)
        res$data <- res$data[i,]
    }
    processing.log.append(res, paste(deparse(match.call()), sep="", collapse=""))
}

stickplot <- function(t, x, y, ...)
{
    ylim <- max(y, na.rm=TRUE) * c(-1, 1)
    plot(range(t), ylim, type="n")
    tstart <- t[1]
    t.isPOSIXlt <- inherits(t, "POSIXlt")
    t.isPOSIXct <- inherits(t, "POSIXct")
    if (t.isPOSIXct) t <- unclass(t)
    if (t.isPOSIXlt) t <- unclass(as.POSIXct(t))
    usr <- par("usr")
    pin <- par("pin")
    tx.scale <- (usr[2]-usr[1]) / (usr[4]-usr[3]) * pin[2] / pin[1]
    n <- length(x)
    xx <- array(dim = 3 * n)
    yy <- array(dim = 3 * n)
    ones <- seq(1, 3*n, 3)
    twos <- seq(2, 3*n, 3)
    threes <- seq(3, 3*n, 3)
    xx[ones] <- t
    yy[ones] <- 0
    xx[twos] <- t + x * tx.scale
    yy[twos] <- y
    xx[threes] <- NA
    yy[threes] <- NA
    lines(xx, yy, type='l', ...)
    ##points(xx[ones],yy[ones],col="red")
}

byte2binary <- function(x)
{
    onebyte2binary <- function(x)
    {
        c("0000","0001","0010","0011",
          "0100","0101","0110","0111",
          "1000","1001","1010","1011",
          "1100","1101","1110", "1111")[x+1]
    }
    rval <- NULL
    if (class(x) == "raw")
        x <- readBin(x, "int", n=length(x), size=1, signed=FALSE)
    for (i in 1:length(x)) {
        if (x[i] < 0) {
            rval <- c(rval, "??")
        } else {
            byte1 <- as.integer(floor(x[i] / 16))
            byte2 <- x[i] - 16 * byte1
            ##cat("input=",x[i],"byte1=",byte1,"byte2=",byte2,"\n")
            rval <- c(rval, paste(onebyte2binary(byte1),
                                  onebyte2binary(byte2), sep=""))
            ##cat(" rval=",rval,"\n")
        }
    }
    rval
}

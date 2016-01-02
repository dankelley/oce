setMethod(f="initialize",
          signature="tidem",
          definition=function(.Object) {
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'tidem' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="tidem",
          definition=function(object, p, constituent, ...) {
              n <- length(object[["p"]])
              ok <- if (!missing(p)) object@data$p <= p else seq(1, n)
              haveP <- any(!is.na(object@data$p))
              if (missing(constituent)) {
                  fit <- data.frame(Const=object@data$const[ok],
                                    Name=object@data$name[ok],
                                    Freq=object@data$freq[ok],
                                    Amplitude=object@data$amplitude[ok],
                                    Phase=object@data$phase[ok],
                                    p=object@data$p[ok])
              } else {
                  i <- which(object@data$name==constituent)
                  if (length(i) == 0)
                      stop("there is no such constituent '", constituent, "'")
                  fit <- data.frame(Const=object@data$const[i],
                                    Name=object@data$name[i],
                                    Freq=object@data$freq[i],
                                    Amplitude=object@data$amplitude[i],
                                    Phase=object@data$phase[i],
                                    p=p)
              }
              cat("tidem summary\n-------------\n")
              cat("\nCall:\n")
              cat(paste(deparse(object[["call"]]), sep="\n", collapse="\n"), "\n", sep="")
              cat("RMS misfit to data: ", sqrt(var(object[["model"]]$residuals)), '\n')
              cat("\nFitted model:\n")
              f <- fit[3:6]
              rownames(f) <- as.character(fit[,2])
              digits <- 3
              if (haveP) {
                  printCoefmat(f, digits=digits,
                               signif.stars=getOption("show.signif.stars"),
                               signif.legend=TRUE,
                               P.values=TRUE, has.Pvalue=TRUE, ...)
              } else {
                  printCoefmat(f[,-4], digits=digits)
              }
              processingLogShow(object)
              invisible(NULL)
          })


setMethod(f="[[",
          signature(x="tidem", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              ## 'j' can be for times, as in OCE
              ##if (!missing(j)) cat("j=", j, "*****\n")
              if (i == "coef") {
                  x@data$model$coef
              } else {
                  ## I use 'as' because I could not figure out callNextMethod() etc
                  ##as(x, "oce")[[i, j, drop]]
                  as(x, "oce")[[i]]
              }
          })

setMethod(f="plot",
          signature=signature("tidem"),
          definition=function(x,
                              which=1,
                              labelIf=NULL,
                              log="",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1,mgp[1]+1,mgp[2]+0.25,mgp[2]+1),
                              ...)
          {
              drawConstituent <- function(name="M2",frequency,col="blue",side=1, adj=NULL)
              {
                  abline(v=frequency, col=col, lty="dotted")
                  if (frequency <= par('usr')[2]) {
                      if (is.null(adj))
                          mtext(name, side=side, at=frequency, col=col, cex=0.8)
                      else
                          mtext(name, side=side, at=frequency, col=col, cex=0.8, adj=adj)
                  }
              }
              drawConstituents <- function(amplitude, type="standard", labelIf=NULL, col="blue")
              {
                  if (type == "standard") {
                      drawConstituent("SA", 0.0001140741, side=3)
                      drawConstituent("O1", 0.0387306544, side=3, adj=1)
                      drawConstituent("K1", 0.0417807462, side=1, adj=0)
                      drawConstituent("M2", 0.0805114007, side=3, adj=1)
                      drawConstituent("S2", 0.0833333333, side=1, adj=0)
                      drawConstituent("M4", 0.1610228013, side=3)
                  } else {
                      if (is.null(labelIf)) labelIf <- amplitude[order(amplitude, decreasing=TRUE)[3]]
                      for (i in 1:nc) {
                          if (amplitude[i] >= labelIf) {
                              abline(v=frequency[i], col=col, lty="dotted")
                              mtext(name[i], side=3, at=frequency[i], col=col)
                          }
                      }
                  }
              }
              if (!inherits(x, "tidem"))
                  stop("method is only for objects of class '", "tidem", "'")
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              if (lw > 1) on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              frequency <- x@data$freq[-1] # trim z0
              amplitude <- x@data$amplitude[-1]
              name      <- x@data$name[-1]
              nc <- length(frequency)
              for (w in 1:lw) {
                  if (which[w] == 2) {
                      plot(frequency, amplitude, col="white", xlab="Frequency [ cph ]", ylab="Amplitude [ m ]", log=log)
                      segments(frequency, 0, frequency, amplitude)
                      drawConstituents(amplitude)
                  } else if (which[w] == 1) {
                      plot(frequency, cumsum(amplitude), xlab="Frequency [ cph ]", ylab="Amplitude [ m ]", log=log, type='s')
                      drawConstituents(amplitude)
                  } else {
                      stop("unknown value of which ", which, "; should be 1 or 2")
                  }
              }
              ##mtext(x$call, side=4, adj=1, cex=2/3)
              if (!all(is.na(pmatch(names(list(...)), "main")))) title(...)
          })


tidemVuf <- function(t, j, lat=NULL)
{
    debug <- 0
    data("tidedata", package="oce", envir=environment())
    tidedata <- get("tidedata")#, pos=globalenv())

    a <- tidemAstron(t)

    if (debug > 0) print(a)

    doodson <- cbind(tidedata$const$d1,
                     tidedata$const$d2,
                     tidedata$const$d3,
                     tidedata$const$d4,
                     tidedata$const$d5,
                     tidedata$const$d6)

    ##v=rem( const.doodson*astro+const.semi, 1);
    oceDebug(debug,
              "doodson[1,]=",doodson[1,],"\n",
              "doodson[2,]=",doodson[2,],"\n",
              "doodson[3,]=",doodson[3,],"\n")
    v <- doodson %*% a$astro + tidedata$const$semi
    oceDebug(debug, "tidedata$const$semi[",j,"]=",tidedata$const$semi[j],"\n")
    v <- v - trunc(v)
    oceDebug(debug, "v[1:3]=",v[1:3],"\n")
    if (!is.null(lat) && !is.na(lat)) {
        if (abs(lat) < 5) lat <- sign(lat) * 5
        slat <- sin(pi * lat / 180)
        k <- which(tidedata$sat$ilatfac == 1)
        rr    <- tidedata$sat$amprat
        rr[k] <- rr[k] * 0.36309 * (1.0 - 5.0 * slat * slat) / slat
        k     <- which(tidedata$sat$ilatfac == 2)
        rr[k] <- rr[k] * 2.59808 * slat

        uu <- tidedata$sat$deldood %*% a$astro[4:6] + tidedata$sat$phcorr
        uu <- uu - trunc(uu)

        oceDebug(debug, "uu[1:3]=",uu[1:3], "\n")

        nsat <- length(tidedata$sat$iconst)
        ##nfreq <- length(tidedata$const$numsat)
        ## loop, rather than make a big matrix
        oceDebug(debug,
                  "tidedata$sat$iconst=", tidedata$sat$iconst, "\n",
                  "length(sat$iconst)=", length(tidedata$sat$iconst),"\n")
        fsum.vec <- vector("numeric", nsat)
        u.vec <- vector("numeric", nsat)
        for (isat in 1:nsat) {
            oceDebug(debug, "isat=",isat,"\n")
            use <- tidedata$sat$iconst == isat
            fsum.vec[isat] <- 1 + sum(rr[use] * exp(1i * 2 * pi * uu[use]))
            u.vec[isat] <- Arg(fsum.vec[isat]) / 2 / pi
            if (isat==8 && debug > 0) {
                cat("TEST at isat=8:\n")
                cat("fsum.vec[",isat,"]=",fsum.vec[isat]," (EXPECT  1.18531604917590 - 0.08028013402313i)\n")
                cat("u.vec[   ",isat,"]=",u.vec[isat],"       (EXPECT -0.01076294959868)\n")
            }
        }
        oceDebug(debug,
                  "uvec[",j,"]=", u.vec[j], "\n",
                  "fsum.vec[",j,"]=", fsum.vec[j],"\n")
        f <- abs(fsum.vec)
        u <- Arg(fsum.vec)/2/pi
        oceDebug(debug, "f=",f,"\n") # FIXME
        oceDebug(debug, "u=",u,"\n") # FIXME

        for (k in which(!is.na(tidedata$const$ishallow))) {
            ik <- tidedata$const$ishallow[k] + 0:(tidedata$const$nshallow[k] - 1)
            f[k] <- prod(f[tidedata$shallow$iname[ik]]^abs(tidedata$shallow$coef[ik]))
            u[k] <- sum(u[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            v[k] <- sum(v[tidedata$shallow$iname[ik]]*tidedata$shallow$coef[ik])
            if (debug>0 && k < 28) cat("k=",k,"f[k]=",f[k]," u[k]=",u[k],"v[k]=",v[k],"\n")
        }
        u <- u[j]
        v <- v[j]
        f <- f[j]
    }
    else {
        v <- v[j]
        u <- rep(0, length(j))
        f <- rep(1, length(j))
    }
    if (length(v) < length(u)) {
        warning("trimming u and f to get same length as v -- this is a bug")
        u <- u[1:length(v)]
        f <- f[1:length(v)]
    }
    list(v=v, u=u, f=f)
}

                                        #function [v,u,f]=tVuf(ctime,ju,lat);
                                        #% T_VUF Computes nodal modulation corrections.
                                        #% [V,U,F]=T_VUF(DATE,JU,LAT) returns the astronomical phase V, the
                                        #% nodal phase modulation U, and the nodal amplitude correction F at
                                        #% a decimal date DATE for the components specified by index JU (into
                                        #% the CONST structure returned by T_GETCONSTS) at a latitude LAT.
                                        #%
                                        #% If LAT is not specified, then the Greenwich phase V is computed with
                                        #% U=0 and F=1.
                                        #%
                                        #% Note that V and U are in 'cycles', not degrees or radians (i.e.,
                                        #% multiply by 360 to get degrees).
                                        #%
                                        #% If LAT is set to NaN, then the nodal corrections are computed for all
                                        #% satellites that do *not* have a "latitude-dependent" correction
                                        #% factor. This is for compatibility with the ways things are done in
                                        #% the xtide package. (The latitude-dependent corrections were zeroed
                                        #% out there partly because it was convenient, but this was rationalized
                                        #% by saying that since the forcing of tides can occur at latitudes
                                        #% other than where they are observed, the idea that observations have
                                        #% the equilibrium latitude-dependence is possibly bogus anyway).
                                        #
                                        #% R. Pawlowicz 11/8/99
                                        #%               1/5/00 - Changed to allow for no LAT setting.
                                        #%              11/8/00 - Added the LAT=NaN option.
                                        #% Version 1.0
                                        #
                                        #% Get all the info about constituents.
                                        #
                                        #[const,sat,shallow]=t_getconsts(ctime);
                                        #
                                        #% Calculate astronomical arguments at mid-point of data time series.
                                        #[astro,ader]=t_astron(ctime);
                                        #
                                        #
                                        #% Phase relative to Greenwich (in units of cycles, presumeably).
                                        #% (This only returns values when we have doodson#s, i.e., not for the
                                        #% shallow water components, but these will be computed later.)
                                        #v=rem( const.doodson*astro+const.semi, 1);
                                        #
                                        #if nargin==3, % If we have a latitude, get nodal corrections.
                                        #
                                        #  % Apparently the second-order terms in the tidal potential go to zero
                                        #  % at the equator, but the third-order terms do not. Hence when trying
                                        #  % to infer the third-order terms from the second-order terms, the
                                        #  % nodal correction factors blow up. In order to prevent this, it is
                                        #  % assumed that the equatorial forcing is due to second-order forcing
                                        #  % OFF the equator, from about the 5 degree location. Latitudes are
                                        #  % hence (somewhat arbitrarily) forced to be no closer than 5 deg to
                                        #  % the equator.
                                        #
                                        #  if finite(lat) & (abs(lat)<5); lat=sign(lat).*5; end
                                        #
                                        #  slat=sin(pi.*lat./180);
                                        #  % Satellite amplitude ratio adjustment for latitude.
                                        #
                                        #  rr=sat.amprat;           % no amplitude correction
                                        #
                                        #  if finite(lat),
                                        #    j=find(sat.ilatfac==1); % latitude correction for diurnal constituents
                                        #    rr(j)=rr(j).*0.36309.*(1.0-5.0.*slat.*slat)./slat;
                                        #
                                        #    j=find(sat.ilatfac==2); % latitude correction for semi-diurnal constituents
                                        #    rr(j)=rr(j).*2.59808.*slat;
                                        #  else
                                        #    rr(sat.ilatfac>0)=0;
                                        #  end;
                                        #
                                        #  % Calculate nodal amplitude and phase corrections.
                                        #
                                        #  uu=rem( sat.deldood*astro(4:6)+sat.phcorr, 1);
                                        #
                                        #  %%uu=uudbl-round(uudbl);  <_ I think this was wrong. The original
                                        #  %                         FORTRAN code is:  IUU=UUDBL
                                        #  %                                           UU=UUDBL-IUU
                                        #  %                         which is truncation.
                                        #
                                        #
                                        #  % Sum up all of the satellite factors for all satellites.
                                        #
                                        #  nsat=length(sat.iconst);
                                        #  nfreq=length(const.isat);
                                        #
                                        #  fsum=1+sum(sparse([1:nsat],sat.iconst,rr.*exp(i*2*pi*uu),nsat,nfreq)).';
                                        #
                                        #  f=abs(fsum);
                                        #  u=angle(fsum)./(2.*pi);
                                        #
                                        #  % Compute amplitude and phase corrections for shallow water constituents.
                                        #
                                        #  for k=find(finite(const.ishallow))',
                                        #    ik=const.ishallow(k)+[0:const.nshallow(k)-1];
                                        #    f(k)=prod(f(shallow.iname(ik)).^abs(shallow.coef(ik)));
                                        #    u(k)=sum( u(shallow.iname(ik)).*shallow.coef(ik) );
                                        #    v(k)=sum( v(shallow.iname(ik)).*shallow.coef(ik) );
                                        #  end;
                                        #
                                        #  f=f(ju);
                                        #  u=u(ju);
                                        #  v=v(ju);
                                        #
                                        #else % Astronomical arguments only.
                                        #
                                        #  % Compute for shallow water constituents.
                                        #  for k=find(finite(const.ishallow))',
                                        #    ik=const.ishallow(k)+[0:const.nshallow(k)-1];
                                        #    v(k)=sum( v(shallow.iname(ik)).*shallow.coef(ik) );
                                        #  end;
                                        #  v=v(ju);
                                        #  f=ones(size(v));
                                        #  u=zeros(size(v));
                                        #end;


tidemAstron <- function(t)
{
                                        # Code mimics t_astron in t_tide
    debug <- FALSE
    d <- as.numeric(difftime(t, ISOdatetime(1899,12,31,12,0,0,tz="UTC"), units="days"))
    D <- d / 10000
    a <- matrix(c(1, d, D^2, D^3), 4, 1)

    oceDebug(debug, "d=",formatC(d,digits=10),"D=",D,"a=", a, "\n")

    sc.hc.pc.np.pp <-
        matrix(c(270.434164, 13.1763965268,-0.0000850, 0.000000039,
                 279.696678,  0.9856473354, 0.00002267,0.000000000,
                 334.329556,  0.1114040803,-0.0007739,-0.00000026 ,
                 -259.183275, 0.0529539222,-0.0001557,-0.000000050,
                 281.220844,  0.0000470684, 0.0000339, 0.000000070),
               nrow=5, ncol=4, byrow=TRUE)
    astro <- ((sc.hc.pc.np.pp %*% a) / 360) %% 1

    oceDebug(debug, "astro=",astro,"\n")

    rem <- as.numeric(difftime(t, trunc.POSIXt(t,units="days"), tz="UTC", units="days"))

    oceDebug(debug, "rem2=",rem,"\n")

    tau <- rem + astro[2,1] - astro[1,1]
    astro <- c(tau, astro)
    da <- matrix(c(0, 1, 2e-4*D, 3e-4*D^2), 4, 1)
    ader <- (sc.hc.pc.np.pp %*% da) / 360
    dtau <- 1 + ader[2,1] - ader[1,1]
    ader <- c(dtau, ader)
    data.frame(astro=astro, ader=ader)
}

tidem <- function(t, x, constituents, latitude=NULL, rc=1, regress=lm,
                  debug=getOption("oceDebug"))
{
    oceDebug(debug, "tidem(t, x, constituents,",
             "latitude=", if(is.null(latitude)) "NULL" else latitude, ", rc, debug) {\n", sep="", unindent=1)
    if (missing(t))
        stop("must supply 't', either a vector of times or a sealevel object")
    if (inherits(t, "sealevel")) {
        sl <- t
        t <- sl[["time"]]
        x <- sl[["elevation"]]
        if (is.null(latitude))
            latitude <- sl[["latitude"]]
    } else {
        if (missing(x))
            stop("must supply 'x', since the first argument is not a sealevel object")
        if (inherits(x, "POSIXt")) {
            warning("tidem() switching first 2 args to permit old-style usage\n")
            tmp <- x
            x <- t
            t <- tmp
        }
        if (length(x) != length(t))
            stop("lengths of 'x' and 't' must match, but they are ", length(x), " and ", length(t), " respectively")
        if (inherits(t, "POSIXt")) {
            t <- as.POSIXct(t)
        } else {
            stop("t must be a vector of POSIXt times")
        }
        sl <- as.sealevel(x, t)
    }

    cl <- match.call()
    startTime <- t[1]
    endTime <- tail(t, 1)
    centralTime <- numberAsPOSIXct((as.numeric(startTime)+as.numeric(endTime))/2, tz=attr(startTime, "tzone"))
    years <- as.numeric(difftime(endTime,startTime, units="secs")) / 86400 / 365.25
    if (years > 18.6)
        warning("Time series spans 18.6 years, but tidem() is ignoring this important fact")

    data("tidedata", package="oce", envir=environment())
    tidedata <- get("tidedata")#, pos=globalenv())
    tc <- tidedata$const
    ntc <- length(tc$name)

    if (debug > 0)
        print(tc)

    name <- freq <- kmpr <- NULL
    indices <- NULL
    standard <- tc$ikmpr > 0
    if (missing(constituents)) {
        name <- tc$name[standard][-1]
        freq <- tc$freq[standard][-1]
        kmpr <- tc$kmpr[standard][-1]
        indices <- c(indices, seq(1:ntc)[standard]) # FIXME: why is Z0 not chopped, as for last 3 lines?
        if (debug > 0)
            print(name)
    } else {
        nconst <- length(constituents)
        for (i in 1:nconst) {
            if (debug > 0)
                cat("[", constituents[i], "]\n",sep="")
            if (constituents[i] == "standard") { # must be first!
                if (i != 1)
                    stop("\"standard\" must occur first in constituents list")
                name <- tc$name[standard][-1]
                freq <- tc$freq[standard][-1]
                kmpr <- tc$kmpr[standard][-1]
                indices <- c(indices, seq(1:ntc)[tc$standard])
            } else {
                if (substr(constituents[i], 1, 1) == "-") {
                    cc <- substr(constituents[i], 2, nchar(constituents[i]))
                    delete <- which(tc$name == cc)
                    if (length(delete) == 1)
                        indices <- indices[indices != delete]
                    else
                        stop("cannot delete constituent '", cc, "' from the list because it is not there")
                } else {
                    add <- which(tc$name == constituents[i])
                    if (length(add) == 1) {
                        if (0 == sum(indices == add)) {
                            indices <- c(indices, add) # avoid duplicates
                        } else {
                            stop("cannot add constituent '", constituents[i], "' because it is not known; see ?tideconst")
                        }
                    }
                }
            }
            if (debug > 0)
                cat("<<", tc$name[indices], ">>\n")
        }
    }
    ## FIXME: what's going on here?  we already have name, etc.  What is tc2 for??
    indices <- indices[order(indices)]
    tc2 <- list(name=tc$name[indices], freq=tc$freq[indices], kmpr=tc$kmpr[indices])

    iZ0 <- which(tc2$name == "Z0")      # Remove Z0
    name <- tc2$name
    if (debug > 0)
        print(name)
    if (length(iZ0)) name <- name[-iZ0]
    nc <- length(name)
    index <- vector("numeric", nc)
    freq <- vector("numeric", nc)
    kmpr <- vector("numeric", nc)

    for (i in 1:nc) {                   # Build up based on constituent names
        ic <- which(tc$name == name[i])
        if (!length(ic))
            stop("there is no tidal constituent named \"", name[i], "\"")
        index[i] <- ic
        freq[i] <- tc$freq[ic]
        kmpr[i] <- tc$kmpr[ic]
    }
    nc <- length(freq)
    ## Check Rayleigh criterion
    interval <- as.numeric(difftime(max(sl@data$time,na.rm=TRUE), min(sl@data$time,na.rm=TRUE), units="hours"))
    drop.term <- NULL
    for (i in 1:nc) {
        cc <- which(tc2$name == kmpr[i])
        if (length(cc)) {
            cannot.fit <- (interval * abs(freq[i]-tc2$freq[cc])) < rc
            ##cat("compare name=", name[i], "with", kmpr[i],":", cannot.fit,"\n")
            if (cannot.fit)
                drop.term <- c(drop.term, i)
        }
    }
    if (length(drop.term) > 0) {
        if (debug > 0)
            cat("Record is too short to fit for constituents:", name[drop.term],"\n")
        index <- index[-drop.term]
        name <- name[-drop.term]
        freq <- freq[-drop.term]
        kmpr <- kmpr[-drop.term]
    }
    nc <- length(freq)
    elevation <- sl[["elevation"]]
    time <- sl[["time"]]
    nt <- length(elevation)
    x <- array(dim=c(nt, 2 * nc))
    x[,1] <- rep(1, nt)
    pi <- 4 * atan2(1, 1)
    ## tRef <- ISOdate(1899, 12, 31, 12, 0, 0, tz="UTC")
    tRef <- centralTime
    hour2pi <- 2 * pi * (as.numeric(time, tz="UTC") - as.numeric(tRef)) / 3600
    oceDebug(debug, "tRef=", tRef, "\n")
    oceDebug(debug, "nc=", nc, "\n")
    ##    cat(sprintf("hour[1] %.3f\n",hour[1]))
    ##    cat(sprintf("hour.offset[1] %.3f\n",hour.offset[1]))
    for (i in 1:nc) {
        oceDebug(debug, "setting coefficients for", name[i], "at", freq[i], "cph", "\n")
        ft <- freq[i] * hour2pi
        x[,2*i-1] <- sin(ft)
        x[,2*i  ] <- cos(ft)
    }
    name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
    dim(name2) <- c(2 * length(name), 1)
    colnames(x) <- name2
    #model <- lm(elevation ~ x, na.action=na.exclude)
    model <- regress(elevation ~ x, na.action=na.exclude)
    if (debug > 0)
        print(summary(model))
    coef  <- model$coefficients
    if (4 == dim(summary(model)$coefficients)[2])
        p.all <- summary(model)$coefficients[,4]
    else
        p.all <- rep(NA, length=1+nc)
    amplitude <- phase <- p <- vector("numeric", length=1+nc)
    ## FIXME: should do offset/trend removal explicitly
    amplitude[1] <- coef[1]
    phase[1] <- 0
    p[1] <- p.all[1]
    for (i in seq.int(2,nc+1)) {
        is <- 2 * (i - 1)
        ic <- 2 * (i - 1) + 1
        s <- coef[is]                   # coefficient on sin(t)
        c <- coef[ic]                   # coefficient on cos(t)
        if (debug > 0)
            cat(name[i-1], "gives s=",s,"and c=",c,"\n")
        amplitude[i] <- sqrt(s^2 + c^2)
        ## Calculate phase from the coefficients on sin() and cos().  Generally,
        ##    cos(t - phase) == cos(phase)*cos(t) + sin(phase)*sin(t)
        ## By the definition of the regression model, we have
        ##    cos(t - phase) == c * cos(t) + s * sin(t)
        ## and thus phase is defined by
        ##    tan(phase) == s/c
        phase[i] <- atan2(s, c)
        ## Adjust amplitude phase, as in ~/src/foreman/tide12_r2.f:405
        j <- which(tidedata$const$name==name[i-1])
        vuf <- tidemVuf(tRef, j=j, lat=latitude)
        amplitude[i] <- amplitude[i] / vuf$f
        phaseOffset <- (vuf$u + vuf$v) * 360 * pi / 180 # the 360 is because tidemVuf returns in cycles
        phase[i] <- phase[i] + phaseOffset 
        p[i] <- 0.5 * (p.all[is] + p.all[ic])
        if (debug > 0)
            cat(name[i-1], "F=", vuf$f, "angle adj=", (vuf$u+vuf$v)*360, "; amp=", amplitude[i], " phase=", phase[i], "\n")
    }
    phase <- phase * 180 / pi
    phase <- ifelse(phase < -360, 720 + phase, phase)
    phase <- ifelse(phase < 0, 360 + phase, phase)

    ## FIXME: if 'inference calculation' is to be done, it should match
    ##     ~/src/t_tide_v1.3beta/t_tide.m:468
    ##     ~/src/foreman/tide12_r2.f:422

    res <- new('tidem')
    res@data <- list(model=model,
                      call=cl,
                      tRef=tRef,
                      const=c(1,   index),
                      name=c("Z0", name),
                      freq=c(0,    freq),
                      amplitude=amplitude,
                      phase=phase,
                      p=p)
    res@metadata$rc <- rc
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


predict.tidem <- function(object, newdata, ...)
{
    if (!missing(newdata) && !is.null(newdata)) {
        ##newdata.class <- class(newdata)
        if (inherits(newdata, "POSIXt")) {
            freq <- object@data$freq[-1]     # drop first (intercept)
            name <- object@data$name[-1]     # drop "z0" (intercept)
            nc <- length(freq)
            tt <- as.numeric(as.POSIXct(newdata, tz="UTC"))
            nt <- length(tt)
            x <- array(dim=c(nt, 2 * nc))
            x[,1] <- rep(1, nt)
            hour2pi <- 2 * pi * (as.numeric(tt) - as.numeric(object[["tRef"]])) / 3600
            for (i in 1:nc) {
                omega.t <- freq[i] * hour2pi
                x[,2*i-1] <- sin(omega.t)
                x[,2*i  ] <- cos(omega.t)
            }
            name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
            dim(name2) <- c(2 * length(name), 1)
            colnames(x) <- name2
            res <- predict(object@data$model, newdata=list(x=x), ...)
        } else {
            stop("newdata must be of class POSIXt")
        }
    } else {
        res <- predict(object@data$model, ...)
    }
    as.numeric(res)
}

webtide <- function(action=c("map", "predict"),
                    longitude, latitude, node, time,
                    basedir=getOption("webtide"),
                    region="nwatl",
                    plot=TRUE, tformat, ...)
{
    action <- match.arg(action)
    subdir <- paste(basedir, "/data/", region, sep="")
    filename <- paste(subdir, "/", region, "_ll.nod", sep="")
    triangles <- read.table(filename, col.names=c("triangle","longitude","latitude"))
    if (action == "map") {
        if (plot) {
            asp <- 1 / cos(pi/180*mean(range(triangles$latitude, na.rm=TRUE)))
            par(mfrow=c(1,1), mar=c(3,3,2,1), mgp=c(2,0.7,0))
            plot(triangles$longitude, triangles$latitude, pch=2, cex=1/4, lwd=1/8,
                 asp=asp, xlab="", ylab="", ...)
            ##usr <- par('usr')
            ##best <- coastlineBest(lonRange=usr[1:2], latRange=usr[3:4])
            warning("tidem: using default coastline for testing")
            data("coastlineWorld", package="oce", envir=environment())
            coastlineWorld <- get("coastlineWorld")
            ##data(best, envir=environment(), debug=debug-1)
            ##coastline <- get(best)
            lines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
            if (missing(node)) {
                point <- locator(1)
                node <- which.min(geodDist(triangles$longitude, triangles$latitude, point$x, point$y))
            }
            longitude <- triangles$longitude[node]
            latitude <- triangles$latitude[node]
            points(longitude, latitude, pch=20, cex=2, col='blue')
            legend("topleft", pch=20, pt.cex=2, cex=3/4, col='blue', bg='white',
                   legend=sprintf("node %.0f %.3fN %.3fE", node, latitude, longitude))
        } else  {
            node <- seq_along(triangles$longitude)
            longitude <- triangles$longitude
            latitude <- triangles$latitude
        }
        return(list(node=node, latitude=latitude, longitude=longitude))
    } else if (action == "predict") {
        if (missing(time))
            time <- seq.POSIXt(from=Sys.time(), by="15 min", length.out=7*4*24)
        if (missing(node)) {
            if (missing(longitude) || missing(latitude))
                stop("'longitude' and 'latitude' must be given unless 'node' is given")
            node <- which.min(geodDist(triangles$longitude, triangles$latitude, longitude, latitude))
        } else {
            latitude <- triangles$latitude[node]
            longitude <- triangles$longitude[node]
        }
        constituentse <- dir(path=subdir, pattern="*.s2c")
        abbrev <- substr(constituentse, 1, 2)
        constituentsuv <- dir(path=subdir, pattern="*.v2c")
        nconstituents <- length(constituentse)
        period <- ampe <- phasee <- ampu <- phaseu <- ampv <- phasev <- vector("numeric", length(nconstituents))
        data("tidedata", package="oce", envir=environment())
        tidedata  <- get("tidedata")#,   pos=globalenv())
        for (i in 1:nconstituents) {
            period[i]  <- 1/tidedata$const$freq[which(abbrev[i] == tidedata$const$name)]
            ## Elevation file contains one entry per node, starting with e.g.:
            ##tri
            ## period 23.934470 (hours) first harmonic 
            ##260.000000 (days) 
            ##1 0.191244 223.820954
            ##2 0.188446 223.141200
            cone <- read.table(paste(subdir,constituentse[i],sep="/"), skip=3)[node,]
            ampe[i] <- cone[[2]]
            phasee[i] <- cone[[3]]
            conuv <- read.table(paste(subdir,constituentsuv[i],sep="/"), skip=3)[node,]
            ampu[i] <- conuv[[2]]
            phaseu[i] <- conuv[[3]]
            ampv[i] <- conuv[[4]]
            phasev[i] <- conuv[[5]]
        }
        ##df <- data.frame(abbrev=abbrev, period=period, ampe=ampe, phasee=phasee, ampu=ampu, phaseu=phaseu, ampv=ampv, phasev=phasev)
        elevation <- u <- v <- rep(0, length(time))
        ## NOTE: tref is the *central time* for tidem()
        tRef <- ISOdate(1899, 12, 31, 12, 0, 0, tz="UTC") 
        h <- (as.numeric(time) - as.numeric(tRef)) / 3600
        for (i in 1:nconstituents) {
            vuf <- tidemVuf(tRef, j=which(tidedata$const$name==abbrev[i]), lat=latitude)
            phaseOffset <- (vuf$u + vuf$v) * 360
            ## NOTE: phase is *subtracted* here, but *added* in tidem()
            elevation <- elevation + ampe[i] * cos((360 * h / period[i] - phasee[i] + phaseOffset) * pi / 180)
            u <- u + ampu[i] * cos((360 * h / period[i] - phaseu[i] + phaseOffset) * pi / 180)
            v <- v + ampv[i] * cos((360 * h / period[i] - phasev[i] + phaseOffset) * pi / 180)
        }
        if (plot) {
            par(mfrow=c(3,1))
            oce.plot.ts(time, elevation, type='l', xlab="", ylab=resizableLabel("elevation"), 
                        main=sprintf("node %.0f %.3fN %.3fE", node, latitude, longitude),
                        tformat=tformat)
            abline(h=0, lty='dotted', col='gray')
            oce.plot.ts(time, u, type='l', xlab="", ylab=resizableLabel("u"),
                        drawTimeRange=FALSE, tformat=tformat)
            abline(h=0, lty='dotted', col='gray')
            oce.plot.ts(time, v, type='l', xlab="", ylab=resizableLabel("v"),
                        drawTimeRange=FALSE, tformat=tformat)
            abline(h=0, lty='dotted', col='gray')
        }
    }
    invisible(list(time=time, elevation=elevation, u=u, v=v,
                   node=node, basedir=basedir, region=region))
}


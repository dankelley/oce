tidem <- function(sl, constituents, latitude=NULL, start.time=NULL, rc=1, quiet = TRUE)
{
    if (!inherits(sl, "sealevel")) stop("method is only for sealevel objects")

    if (is.null(start.time)) start.time <- sl$data$t[which(!is.na(sl$data$t))][1]

    if (!quiet) {
        cat("start.time=")
        print(start.time)
        cat("\n")
    }

    data("tidedata")
    td <- get("tidedata", pos=globalenv())
    tc <- td$const
    ntc <- length(tc$name)

    if (!quiet) print(tc)

    name <- freq <- kmpr <- NULL
    indices <- NULL
    standard <- tc$ikmpr > 0
    if (missing(constituents)) {
        name <- tc$name[standard][-1]
        freq <- tc$freq[standard][-1]
        kmpr <- tc$kmpr[standard][-1]
        indices <- c(indices, seq(1:ntc)[standard])
        if (!quiet) print(name);
    }
    else {
        nconst <- length(constituents)
        for (i in 1:nconst) {
            if (!quiet) cat("[", constituents[i], "]\n",sep="")
            if (constituents[i] == "standard") { # must be first!
                if (i != 1) stop("\"standard\" must occur first in constituents list")
                name <- tc$name[standard][-1]
                freq <- tc$freq[standard][-1]
                kmpr <- tc$kmpr[standard][-1]
                indices <- c(indices, seq(1:ntc)[tc$standard])
            }
            else {
                if (substr(constituents[i], 1, 1) == "-") {
                    cc <- substr(constituents[i], 2, nchar(constituents[i]))
                    delete <- which(tc$name == cc)
                    if (length(delete) == 1) indices <- indices[indices != delete]
                    else stop("cannot delete constituent '", cc, "' from the list because it is not there")
                }
                else {
                    add <- which(tc$name == constituents[i])
                    if (length(add) == 1) {
                        if (0 == sum(indices == add)) indices <- c(indices, add) # avoid duplicates
                    }
                    else
                        stop("cannot add constituent '", constituents[i], "' because it is not known; see ?tideconst")
                }
            }
            if (!quiet) cat("<<", tc$name[indices], ">>\n")
        }
    }
    indices <- indices[order(indices)]
    tc2 <- list(name=tc$name[indices], freq=tc$freq[indices], kmpr=tc$kmpr[indices])

    iZ0 <- which(tc2$name == "Z0")      # Remove Z0
    name <- tc2$name
    if (!quiet) print(name)
    if (length(iZ0)) name <- name[-iZ0]
    nc <- length(name)
    index <- vector("numeric", nc)
    freq <- vector("numeric", nc)
    kmpr <- vector("numeric", nc)

    for (i in 1:nc) {                   # Build up based on constituent names
        ic <- which(tc$name == name[i])
        if (!length(ic)) stop("there is no tidal constituent named \"", name[i], "\"")
        index[i] <- ic
        freq[i] <- tc$freq[ic]
        kmpr[i] <- tc$kmpr[ic]
    }
    nc <- length(freq)
                                        # Check Rayleigh criterion
    interval <- as.numeric(difftime(max(sl$data$t,na.rm=TRUE),min(sl$data$t,na.rm=TRUE),units="hours"))
    drop.term <- NULL
    for (i in 1:nc) {
        cc <- which(tc2$name == kmpr[i])
        if (length(cc)) {
            cannot.fit <- (interval * abs(freq[i]-tc2$freq[cc])) < rc
            ##cat("compare name=", name[i], "with", kmpr[i],":", cannot.fit,"\n")
            if (cannot.fit)	drop.term <- c(drop.term, i)
        }
    }
    if (length(drop.term) > 0) {
        if (!quiet) cat("Record is too short to fit for constituents:", name[drop.term],"\n")
        index <- index[-drop.term]
        name <- name[-drop.term]
        freq <- freq[-drop.term]
        kmpr <- kmpr[-drop.term]
    }

    nc <- length(freq)
    nt <- length(sl$data$eta)
    x <- array(dim=c(nt, 2 * nc))
    x[,1] <- rep(1, nt)

    hour <- unclass(as.POSIXct(sl$data$t, tz="GMT")) / 3600 # seconds since 0000-01-01 00:00:00

    centralindex <- floor(length(sl$data$t) / 2)
    hour.wrt.centre <- unclass(hour - hour[centralindex])
    hour2pi <- 2 * pi * hour.wrt.centre

    for (i in 1:nc) {
        omega.t <- freq[i] * hour2pi
        x[,2*i-1] <- sin(omega.t)
        x[,2*i  ] <- cos(omega.t)
    }
    name2 <- matrix(rbind(paste(name,"_S",sep=""), paste(name,"_C",sep="")), nrow=(length(name)), ncol=2)
    dim(name2) <- c(2 * length(name), 1)
    colnames(x) <- name2
    eta <- sl$data$eta
    model <- lm(eta ~ x, na.action=na.exclude)
    coef  <- model$coefficients
    p.all <- summary(model)$coefficients[,4]
    amplitude <- phase <- p <-vector("numeric", length=1+nc)
    ## FIXME: should do offset/trend removal explicitly
    amplitude[1] <- coef[1]
    phase[1] <- 0
    p[1] <- p.all[1]
    for (i in seq(2,nc+1)) {
        is <- 2 * (i - 1)
        ic <- 2 * (i - 1) + 1
        s <- coef[is]                   # coefficient on sin(t)
        c <- coef[ic]                   # coefficient on cos(t)
        if (!quiet) cat(name[i-1], "gives s=",s,"and c=",c,"\n")
        amplitude[i] <- sqrt(s^2 + c^2)
                                        # sin(t - phase) == cos(phase)*sin(t) - sin(phase)*cos(t)
                                        #                == s * sin(t) + c * cos(t)
                                        # thus tan(phase) is -c/s
        phase[i]     <- atan2(-c, s)    # atan2(y,x)
                                        # FIXME: is the sign right?
        p[i]         <- 0.5 * (p.all[is] + p.all[ic])
    }
    if (!quiet) cat("coef:", coef, "\n")
    phase <- phase * 180 / pi

    centraltime <- as.POSIXct(sl$data$t[1] + 3600*centralindex, tz="GMT")
    if (!quiet) {
        cat("centraltime=")
        print(centraltime)
        cat("\n")
        cat("L199 index:",index,"(length=",length(index),")\n")
    }

    if (is.null(latitude)) latitude <- sl$metadata$latitude
    vuf <- tidem.vuf(centraltime, c(0, index), latitude)
    vu <- c(0, (vuf$v + vuf$u) * 360)
    phase2 <- phase - vu                # FIXME: plus or minus??
    negate <- phase2 < 0
    phase2[negate] <- 360 + phase2[negate]
    phase <- phase2

    if (!quiet) cat("vu=",vu,"\n")

    rval <- list(model=model,
                 const=c(1,   index),
                 name=c("Z0", name),
                 freq=c(0,    freq),
                 amplitude=amplitude,
                 phase=phase,
                 phase2=phase2,         # FIXME: remove later
                 p=p)
    class(rval) <- "tide"
    rval
}


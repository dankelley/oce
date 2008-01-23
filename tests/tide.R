library(oce)
## See page 2 of Foreman 1977 for the format of tide3.dat,
## which is provided in t-tide as (what seems to be) an exact
## copy of the appendix in Foreman (1977).

debug <- 0                              # 0, 1 or 2
file <- file("tide3.dat", "r")

############################
## Constituents [const]   ##
############################

name <- compare <- vector("character", 1000) # will trim later
frequency <- vector("numeric", 1000)
nconst <- 1
while (TRUE) {
    items <- scan(file, "character", nlines=1, quiet=TRUE)
    if (debug > 2) print(items)
    nitems <- length(items)
    if (nitems == 0)
        break;
    if (nitems != 2 && nitems != 3) stop("wrong number of entries on line", nconst)
    compare[nconst]   <- if (nitems == 2) ""  else items[3]
    name[nconst]      <- items[1]
    frequency[nconst] <- as.numeric(items[2])
    nconst <- nconst + 1
}
nconst <- nconst - 1
length(name) <- nconst
length(frequency) <- nconst
length(compare) <- nconst
const <- data.frame(name=name, frequency=frequency, compare=compare, standard=compare!="", stringsAsFactors=FALSE)
##rownames(const) <- name

stopifnot(const$name[1] == "Z0")
stopifnot(const$name[2] == "SA")
stopifnot(const$compare[2] == "SSA")
i <- which(const$name == "M2")
stopifnot(const[i,2] == 1/12.42060119816049912345)

if (debug > -1) {
    cat("\n")
    cat("/-----------------------------------------------------------\\\n")
    cat("| Constituent data in data.frame named 'const' of dim", dim(const), " |\n")
    cat("\\-----------------------------------------------------------/\n")
}

############################
## Satellites [sat]       ##
############################
get.satellite <- function(x, o)
{
    if (debug > 2) cat("  SAT '",x,"'\n",sep="")
    ldel  <- as.numeric(substr(x,o+01,o+03))            # I3
    mdel  <- as.numeric(substr(x,o+04,o+06))            # I3
    ndel  <- as.numeric(substr(x,o+07,o+09))            # I3
    ph    <- as.numeric(substr(x,o+10,o+13))            # F4.2
    ee    <- as.numeric(substr(x,o+14,o+20))            # F7.4
    ir    <- as.numeric(substr(x,o+22,o+22))            # 1x, I1
    if (debug > 2) cat("  SAT  ldel=",ldel, "mdel=",mdel,"ndel=",ndel,"ph=",ph,"ee=",ee,"ir=",ir,"\n")
    c(ldel, mdel, ndel, ph, ee, ir)
}


scan(file, "character", nlines=3, quiet=TRUE) # skip 3 lines

nsat <- 1
sat <- vector("list", 1000)             # will trim later
while (TRUE) {
    x <- readLines(file, n=1)
    nx <- nchar(x)
    if (nx < 10) break
                                        # Line format and content
                                        # 6X,A5,1X,6I3,F5.2,I4
                                        # kon, ii,jj,kk,ll,mm,nn semi nj
    kon <- gsub(" ", "", substr(x, 7, 11))
    ii <- as.numeric(substr(x, 13, 15))
    jj <- as.numeric(substr(x, 16, 18))
    kk <- as.numeric(substr(x, 19, 21))
    ll <- as.numeric(substr(x, 22, 24))
    mm <- as.numeric(substr(x, 25, 27))
    nn <- as.numeric(substr(x, 28, 30))
    semi <- as.numeric(substr(x, 31, 35))
    nj <- as.numeric(substr(x, 36, 39))
    if (debug > 1) {
        cat(">>>", x, "\n", sep="")
        cat("kon=", kon, " ii=",ii," jj=",jj," kk=",kk," ll=",ll," mm=",mm," nn=",nn," semi=",semi," nj=",nj,"\n",sep="")
    }
    ldel <- vector("numeric", nj)
    mdel <- vector("numeric", nj)
    ndel <- vector("numeric", nj)
    ph   <- vector("numeric", nj)
    ee   <- vector("numeric", nj)
    ir   <- vector("numeric", nj)
    if (nj > 0) {

        ## ALP1    1 -4  2  1  0  0 -.25   2
        ## ALP1  -1  0  0 .75 0.0360R1   0 -1  0 .00 0.1906
        is <- 1
        while (is <= nj) {
            xs <- readLines(file, n=1)
            if (debug > 1) cat(">>>", xs, "'\n",sep="")
            nxs <- nchar(xs)
            if (nxs != 31 && nxs != 33 && nxs != 54 && nxs != 56 && nxs != 77 && nxs != 79) {
                cat("GOT BAD LINE AS FOLLOWS:\n12345678901234567890123456789012345678901234567890\n",xs,"\n",sep="")
                stop("need 31, 54, 77 or 79 chars, but got ", nxs)
            }
            s <- get.satellite(xs, 11)
            ldel[is]<-s[1];mdel[is]<-s[2];ndel[is]<-s[3];ph[is]<-s[4];ee[is]<-s[5];ir[is]<-s[6]
            is <- is + 1
            if (nxs > 50) {
                s <- get.satellite(xs, 34)
                ldel[is]<-s[1];mdel[is]<-s[2];ndel[is]<-s[3];ph[is]<-s[4];ee[is]<-s[5];ir[is]<-s[6]
                is <- is + 1
            }
            if (nxs > 70) {
                s <- get.satellite(xs, 57)
                ldel[is]<-s[1];mdel[is]<-s[2];ndel[is]<-s[3];ph[is]<-s[4];ee[is]<-s[5];ir[is]<-s[6]
                is <- is + 1
            }
        }
        if (debug > 1) print(data.frame(ldel=ldel,mdel=mdel,ndel=ndel,ph=ph,ee=ee,ir=ir))
        sat[[nsat]] <- list(name=kon, ii=ii, jj=jj, kk=kk, mm=mm, nn=nn, semi=semi, nj=nj,
                            ldel=ldel, mdel=mdel, ndel=ndel, ph=ph, ee=ee, ir=ir)
    } else {
        sat[[nsat]] <- list(name=kon, ii=ii, jj=jj, kk=kk, mm=mm, nn=nn, semi=semi, nj=nj,
                            ldel=NULL, mdel=NULL, ndel=NULL, ph=NULL, ee=NULL, ir=NULL)
    }
    if (debug>0) cat("\n")
    nsat <- nsat + 1
}
if (nsat < 1) stop("failed to read any satellite entries")

length(sat) <- nsat - 1

## This portion of the file ends as follows
##      M3      3  0  0  0  0  0 -.50   1
##      M3     0 -1  0 .50  .0564
## and see Foreman (1977) page 2 for how to read this.  Here,
## we'll check every aspect of the last satellite.

stopifnot(sat[[44]]$name == "M3")
stopifnot(sat[[44]]$ii == 3)
stopifnot(sat[[44]]$jj == 0)
stopifnot(sat[[44]]$kk == 0)
stopifnot(sat[[44]]$mm == 0)
stopifnot(sat[[44]]$nn == 0)
stopifnot(sat[[44]]$semi == -0.50)
stopifnot(sat[[44]]$nj == 1)
stopifnot(sat[[44]]$ldel == 0)
stopifnot(sat[[44]]$mdel == -1)
stopifnot(sat[[44]]$ndel == 0)
stopifnot(sat[[44]]$ph == 0.50)
stopifnot(sat[[44]]$ee == 0.0564)
stopifnot(is.na(sat[[44]]$ir))

if (debug > -1) {
    cat("\n")
    cat("/---------------------------------------------------------\\\n")
    cat("| Satellite data in list named 'sat' of length", length(sat), "         |\n")
    cat("\\---------------------------------------------------------/\n")
}

##
############################
## Shallow [shallow]      ##
############################

# (6X,A5,I1,2X,4(F5.2,A5,5X))
# KON = name of the shallow water constituent;
# NJ = number of main constituents from which it is derived;
# COEF,KONCO = combination number and name of these main constituents.

shallow <- vector("list", 1000)         # will trim later
nshallow <- 1
while(TRUE) {
    x <- readLines(file, n=1)
    nx <- nchar(x)
    if (nx < 10) break
    kon <- gsub(" ", "", substr(x, 7, 11))
    nj <- as.numeric(substr(x, 12, 12))
    if (debug > 1) cat("kon: '", kon, "' nj=", nj, "\n",sep="")
    coef <- konco <- NULL
    if (nj > 0) {
        for (j in 1:nj) {
            o <- 15 + (j-1)*15
            ##cat("  x='",x,"'\n",sep="")
            ##cat("'", substr(x, o, nx), "'\n",sep="")
            coef  <- c(coef,  as.numeric(substr(x, o, o+4)))
            konco <- c(konco, gsub(" ", "", substr(x, o+5, o+9)))
            ##cat("  coef= ",coef," konco= '",konco,"'\n",sep="")
        }
    }
    shallow[[nshallow]] <- list(name=kon, nj=nj, coef=coef, konco=konco)
    nshallow <- nshallow + 1
}
nshallow <- nshallow - 1
length(shallow) <- nshallow

stopifnot(length(shallow) == 101)
stopifnot(shallow[[101]]$name == "ST35")
stopifnot(shallow[[101]]$nj   == 4)
stopifnot(shallow[[101]]$coef == c(3, 1, 1, 1))
stopifnot(shallow[[101]]$konco == c("M2", "N2", "K2", "S2"))

if (debug > -1) {
    cat("\n")
    cat("/---------------------------------------------------------\\\n")
    cat("| Shallow data in list named 'shallow' of length", length(shallow), " |\n")
    cat("\\---------------------------------------------------------/\n")
}

close(file)
tideconst   <- const
tidesat     <- sat
tideshallow <- shallow

cat("
DO MANUALLY:
    save(tideconst, tidesat, tideshallow, file=\"../data/tidesetup.rda\")
TO SET UP THE SYSTEM.
")

#####################
## Low-level tests ##
#####################
                                        # Test against matlab t_astron
a <- tidem.astron(as.POSIXct("2008-01-22 18:50:24"))
stopifnot(all.equal(a$astro, c(1.2886, 0.3339, 0.8375, 0.1423, 0.0856, 0.7863), 0.001))
stopifnot(all.equal(a$ader,  c(0.9661, 0.0366, 0.0027, 0.0003, 0.0001, 0.0000), 0.001))

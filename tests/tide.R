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
tidedoodson <- matrix(NA, nconst, 6)
tidesemi <- vector("numeric", nconst)
numsat <- vector("numeric", nconst)

stopifnot(name[1] == "Z0")
stopifnot(name[2] == "SA")
stopifnot(compare[2] == "SSA")
i <- which(name == "M2")
stopifnot(frequency[i] == 1/12.42060119816049912345)


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

nsat <- 162
deldood <- matrix(NA, nsat, 3)
phcorr  <- matrix(NA, nsat, 1)
amprat  <- matrix(NA, nsat, 1)
ilatfac <- matrix(NA, nsat, 1)
iconst  <- matrix(NA, nsat, 1)

isat <- 1
while (TRUE) {
    if (debug > 1) cat("***** looking for satellite ", isat, "********\n")
    x <- readLines(file, n=1)
    nx <- nchar(x)
    if (isat > nsat || nx < 10) break
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

    which.constituent <- which(name == kon)
    tidedoodson[which.constituent, ] <- c(ii, jj, kk, ll, mm, nn)
    if (debug > 0) cat("name=", kon, "w=", w,"\n")

    this.semi <- as.numeric(substr(x, 31, 35))
    tidesemi[which.constituent] <- this.semi
    nj <- as.numeric(substr(x, 36, 39)) # number of satellites

    if (debug > 1) cat("------------ nj=", nj, "-------------\n")

    numsat[which.constituent] <- nj
    if (debug > 1) {
        cat(">>>", x, "\n", sep="")
        cat("kon=", kon, " ii=",ii," jj=",jj," kk=",kk," ll=",ll," mm=",mm," nn=",nn," this.semi=",this.semi," nj=",nj,"\n",sep="")
    }
    if (nj > 0) {
        ## ALP1    1 -4  2  1  0  0 -.25   2
        ## ALP1  -1  0  0 .75 0.0360R1   0 -1  0 .00 0.1906
        is <- 1
        while (is <= nj) {
            xs <- readLines(file, n=1)
            if (debug > 1) cat(">>>", xs, "<<<\n",sep="")
            nxs <- nchar(xs)
            if (nxs != 31 && nxs != 33 && nxs != 39 && nxs != 54 && nxs != 56 && nxs != 77 && nxs != 79) {
                cat("GOT BAD LINE AS FOLLOWS:\n12345678901234567890123456789012345678901234567890\n",xs,"\n",sep="")
                stop("need 31, 33, 39, 54, 56, 77 or 79 chars, but got ", nxs)
            }
            s <- get.satellite(xs, 11)
            deldood[isat,1:3] <- s[1:3]
            print(deldood[isat, 1:3])
            phcorr[isat]  <- s[4]
            amprat[isat]  <- s[5]
            ilatfac[isat] <- s[6]
            iconst[isat]  <- which.constituent # constituent to which this satellite is attached
            if (debug > 1) cat("Got satellite ", isat, "for constituent", kon, "(", which.constituent, ") which has amprat", amprat[isat], "\n")
            isat <- isat + 1
            is <- is + 1
            if (nxs > 50) {
                s <- get.satellite(xs, 34)
                deldood[isat, 1:3] <- s[1:3]
                phcorr[isat]  <- s[4]
                amprat[isat]  <- s[5]
                ilatfac[isat] <- s[6]
                iconst[isat]  <- which.constituent # constituent to which this satellite is attached
                if (debug > 1) cat("Got satellite ", isat, "for constituent", kon, "(", which.constituent, "), which has amprat", amprat[isat], "\n")
                isat <- isat + 1
                is <- is + 1
            }
            if (nxs > 70) {
                s <- get.satellite(xs, 57)
                deldood[isat, 1:3] <- s[1:3]
                phcorr[isat]  <- s[4]
                amprat[isat]  <- s[5]
                ilatfac[isat] <- s[6]
                iconst[isat]  <- which.constituent # constituent to which this satellite is attached
                if (debug > 1) cat("Got satellite ", isat, "for constituent", kon, "(", which.constituent, "), which has amprat", amprat[isat], "\n")
                isat <- isat + 1
                is <- is + 1
            }
        }
    }
    if (debug>0) cat("\n")
}
if ((isat - 1) != nsat) stop("failed to read all ", nsat, " satellite entries.  Only got ", isat)



stopifnot(sum(numsat) == nsat)

tidesat <- list(deldood=deldood, phcorr=phcorr, amprat=amprat, ilatfac=ilatfac, iconst=iconst)

cat("ok?\n")
print(tidesat$deldood[1,])
print(tidesat$deldood[2,])
print(tidesat$deldood[3,])
cat("ok?\n")
stopifnot(tidesat$deldood[1,] == c(-1,  0, 0))
stopifnot(tidesat$deldood[2,] == c( 0, -1, 0))
stopifnot(tidesat$deldood[3,] == c(-2, -2, 0))

tideconst <- data.frame(name=name, frequency=frequency, compare=compare, semi=tidesemi, numsat=numsat, standard=compare!="", stringsAsFactors=FALSE)

stopifnot(tideconst$numsat[48] == 9)        # M2

if (debug > -1) {
    cat("\n")
    cat("/---------------------------------------------------------------\\\n")
    cat("| Constituent data in data.frame named 'tideconst' of dim", dim(tideconst), "|\n")
    cat("\\---------------------------------------------------------------/\n")
}

## This portion of the file ends as follows
##      M3      3  0  0  0  0  0 -.50   1
##      M3     0 -1  0 .50  .0564
## and see Foreman (1977) page 2 for how to read this.  Here,
## we'll check every aspect of the last satellite.

if (debug > -1) {
    cat("\n")
    cat("/------------------------------------------------------------\\\n")
    cat("| Satellite data in list named 'tidesat' with ", length(tidesat$amprat), " elements |\n")
    cat("\\------------------------------------------------------------/\n")
}

#############
## Doodson ##
#############
tidedoodson[1,] <- rep(0, 6)
stopifnot(tidedoodson[48,] == c(2, 0, 0, 0, 0, 0))
if (debug > -1) {
    cat("\n")
    cat("/---------------------------------------------------------\\\n")
    cat("| Doodson data in matrix named 'tidedoodson' of dim", dim(tidedoodson), "|\n")
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

tideshallow <- vector("list", 1000)         # will trim later
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
    tideshallow[[nshallow]] <- list(name=kon, nj=nj, coef=coef, konco=konco)
    nshallow <- nshallow + 1
}
nshallow <- nshallow - 1
length(tideshallow) <- nshallow

stopifnot(length(tideshallow) == 101)
stopifnot(tideshallow[[101]]$name == "ST35")
stopifnot(tideshallow[[101]]$nj   == 4)
stopifnot(tideshallow[[101]]$coef == c(3, 1, 1, 1))
stopifnot(tideshallow[[101]]$konco == c("M2", "N2", "K2", "S2"))

if (debug > -1) {
    cat("\n")
    cat("/--------------------------------------------------------\\\n")
    cat("| Shallow data in list named 'tideshallow' of length", length(tideshallow), "|\n")
    cat("\\--------------------------------------------------------/\n")
}

close(file)

cat("
DO MANUALLY:
    save(tideconst, tidedoodson, tidesat, tideshallow, file=\"../data/tidesetup.rda\")
TO SET UP THE SYSTEM.
")

#####################
## Low-level tests ##
#####################
                                        # Test against matlab t_astron
t <- as.POSIXct("2008-01-22 18:50:24")
a <- tidem.astron(t)
stopifnot(all.equal(a$astro, c(1.2886, 0.3339, 0.8375, 0.1423, 0.0856, 0.7863), 0.001))
stopifnot(all.equal(a$ader,  c(0.9661, 0.0366, 0.0027, 0.0003, 0.0001, 0.0000), 0.001))

vuf <- tidem.vuf(t, 48)
stopifnot(all.equal(c(vuf$v), c(0.57722632857477)))
stopifnot(all.equal(c(vuf$u), c(0)))
stopifnot(all.equal(c(vuf$f), c(1)))

#vuf <- tidem.vuf(t, 48, 45)
#stopifnot(all.equal(c(vuf$v), c(0.57722632857477)))
#stopifnot(all.equal(c(vuf$u), c(0.00295677805220)))
#stopifnot(all.equal(c(vuf$f), c(0.96893771510868)))

# ISSUES
# matlab has nsat=162 but R has 44.
# sat.amprat seems to be sat$ee

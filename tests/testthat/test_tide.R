library(oce)

context("tides")

## See page 2 of Foreman 1977 for the format of tide3.dat,
## which is provided in t-tide as (what seems to be) an exact
## copy of the appendix in Foreman (1977).

## Notes
## 1. most of the tests here check against the T_TIDE values.
## 2. variable names are chosen to match T_TIDE, except that e.g. "const" is "tideconst"

file <- file("tide3.dat.gz", "r")

############################
## Constituents [const]   ##
############################

nc <- 146
name <- kmpr <- vector("character", nc)
freq <- ikmpr <- df <-
    d1 <- d2 <- d3 <- d4 <- d5 <- d6 <-
    semi <- isat <- nsat <- ishallow <- nshallow <- doodsonamp <- doodsonspecies <-
    vector("numeric", nc)
doodsonamp <- rep(NA, nc)
doodsonspecies <- rep(NA, nc)

ishallow <- NA
ic <- 1
while (TRUE) {
    items <- scan(file, "character", nlines=1, quiet=TRUE)
    nitems <- length(items)
    if (nitems == 0)
        break
    if (nitems != 2 && nitems != 3) stop("wrong number of entries on line", ic)
    name[ic] <- gsub(" ", "", items[1])
    freq[ic] <- as.numeric(items[2])
    kmpr[ic] <- if (nitems == 2) ""  else gsub(" ", "", items[3])
    ic <- ic + 1
}
for (ic in 1:nc) {
    if (kmpr[ic] != "")  {
        ikmpr[ic] <- which(name == kmpr[ic])
        df[ic]    <- freq[ic] - freq[ikmpr[ic]]
    }
}
df[1] <- 0                              # Z0 is special (???) BUG: fixme

############################
## Satellites [sat]       ##
############################
get.satellite <- function(x, o)
{
    ldel  <- as.numeric(substr(x,o+01,o+03))            # I3
    mdel  <- as.numeric(substr(x,o+04,o+06))            # I3
    ndel  <- as.numeric(substr(x,o+07,o+09))            # I3
    ph    <- as.numeric(substr(x,o+10,o+13))            # F4.2
    ee    <- as.numeric(substr(x,o+14,o+20))            # F7.4
    ir    <- as.numeric(substr(x,o+22,o+22))            # 1x, I1
    c(ldel, mdel, ndel, ph, ee, ir)
}

scan(file, "character", nlines=3, quiet=TRUE) # skip 3 lines

ns <- 162
deldood <- matrix(NA, ns, 3)
phcorr  <- matrix(NA, ns, 1)
amprat  <- matrix(NA, ns, 1)
ilatfac <- matrix(NA, ns, 1)
iconst  <- matrix(NA, ns, 1)

this.sat <- 1
while (TRUE) {
    x <- readLines(file, n=1)
    nx <- nchar(x)
    if (this.sat > ns || nx < 10) break
    ## Line format and content
    ## 6X,A5,1X,6I3,F5.2,I4
    ## kon, ii,jj,kk,ll,mm,nn semi nj
    kon <- gsub(" ", "", substr(x, 7, 11))
    which.constituent <- which(name == kon)

    d1[which.constituent] <- ii <- as.numeric(substr(x, 13, 15))
    d2[which.constituent] <- jj <- as.numeric(substr(x, 16, 18))
    d3[which.constituent] <- kk <- as.numeric(substr(x, 19, 21))
    d4[which.constituent] <- ll <- as.numeric(substr(x, 22, 24))
    d5[which.constituent] <- mm <- as.numeric(substr(x, 25, 27))
    d6[which.constituent] <- nn <- as.numeric(substr(x, 28, 30))

    semi[which.constituent] <- as.numeric(substr(x, 31, 35))
    nj <- as.numeric(substr(x, 36, 39)) # number of satellites
    nsat[which.constituent] <- nj

    if (nj > 0) {
        ## ALP1    1 -4  2  1  0  0 -.25   2
        ## ALP1  -1  0  0 .75 0.0360R1   0 -1  0 .00 0.1906
        is <- 1
        while (is <= nj) {
            xs <- readLines(file, n=1)
            nxs <- nchar(xs)
            if (nxs != 31 && nxs != 33 && nxs != 39 && nxs != 54 && nxs != 56 && nxs != 77 && nxs != 79) {
                cat("GOT BAD LINE AS FOLLOWS:\n12345678901234567890123456789012345678901234567890\n",xs,"\n",sep="")
                stop("need 31, 33, 39, 54, 56, 77 or 79 chars, but got ", nxs)
            }
            s <- get.satellite(xs, 11)
            deldood[this.sat, 1:3] <- s[1:3]
            phcorr[this.sat]  <- s[4]
            amprat[this.sat]  <- s[5]
            ilatfac[this.sat] <- s[6]
            iconst[this.sat]  <- which.constituent # constituent to which this satellite is attached
            this.sat <- this.sat + 1
            is <- is + 1
            if (nxs > 50) {
                s <- get.satellite(xs, 34)
                deldood[this.sat, 1:3] <- s[1:3]
                phcorr[this.sat]  <- s[4]
                amprat[this.sat]  <- s[5]
                ilatfac[this.sat] <- s[6]
                iconst[this.sat]  <- which.constituent # constituent to which this satellite is attached
                this.sat <- this.sat + 1
                is <- is + 1
            }
            if (nxs > 70) {
                s <- get.satellite(xs, 57)
                deldood[this.sat, 1:3] <- s[1:3]
                phcorr[this.sat]  <- s[4]
                amprat[this.sat]  <- s[5]
                ilatfac[this.sat] <- s[6]
                iconst[this.sat]  <- which.constituent # constituent to which this satellite is attached
                this.sat <- this.sat + 1
                is <- is + 1
            }
        }
    }
}
if ((this.sat - 1) != ns) stop("failed to read all ", ns, " satellite entries.  Only got ", this.sat)


sat <- list(deldood=deldood, phcorr=phcorr, amprat=amprat, ilatfac=ilatfac, iconst=iconst)


## This portion of the file ends as follows
##      M3      3  0  0  0  0  0 -.50   1
##      M3     0 -1  0 .50  .0564
## and see Foreman (1977) page 2 for how to read this.  Here,
## we'll check every aspect of the last satellite.

##
############################
## Shallow [shallow]      ##
############################

## (6X,A5,I1,2X,4(F5.2,A5,5X))
## KON = name of the shallow water constituent;
## NJ = number of main constituents from which it is derived;
## COEF,KONCO = combination number and name of these main constituents.

num.shallow <- 251
iconst <- vector("numeric", num.shallow)   # names as T_TIDE
coef   <- vector("numeric", num.shallow)
iname  <- vector("numeric", num.shallow)
this.shallow <- 1
while(TRUE) {
    x <- readLines(file, n=1)
    nx <- nchar(x)
    if (nx < 10) break
    kon <- gsub(" ", "", substr(x, 7, 11))
    which.constituent <- which(name == kon)
    nj <- as.numeric(substr(x, 12, 12))
    nshallow[which.constituent] <- nj
    if (nj > 0) {
        for (j in 1:nj) {
            o <- 15 + (j-1)*15
            iconst[this.shallow] <- which.constituent
            coef[this.shallow]  <- as.numeric(substr(x, o, o+4))
            konco <- gsub(" ", "", substr(x, o+5, o+9))
            iname[this.shallow] <- which(name == konco)
            ishallow[which.constituent] <- this.shallow - j + 1 # BUG: broken
            this.shallow <- this.shallow + 1
        }
    }
}
close(file)

shallow <- data.frame(iconst=iconst, coef=coef, iname=iname)


#################
## equilibrium ##
##################

efile <- file("t_equilib.dat.gz", "r")
edat <- readLines(efile)
ne <- length(edat)
for (i in 10:ne) {                      # 9 lines of header
    # Name Species A B
    kon <- gsub(" ", "", substr(edat[i], 1, 4))
    which.constituent <- which(name == kon)
    if (length(which.constituent) < 1) stop("cannot understand equilibirum constituent", kon)
    species <- as.numeric(substr(edat[i], 8, 8))
    A <- as.numeric(substr(edat[i],  9, 15))
    B <- as.numeric(substr(edat[i], 16, 21))
    if (A != 0) {
        doodsonamp[which.constituent] <- A / 1e5
        doodsonspecies[which.constituent] <- species
    } else {
        doodsonamp[which.constituent] <- B / 1e5
        doodsonspecies[which.constituent] <- -species
    }
}

close(efile)

const <- data.frame(name=name,
                    freq=freq,
                    kmpr=kmpr,
                    ikmpr=ikmpr,
                    df=df,
                    d1=d1,d2=d2,d3=d3,d4=d4,d5=d5,d6=d6, # T_TIDE has these as matrix 'doodson'
                    semi=semi,
                    nsat=nsat,
                    ishallow=ishallow,
                    nshallow=nshallow,
                    doodsonamp=doodsonamp,
                    doodsonspecies=doodsonspecies,
                    stringsAsFactors=FALSE)
tidedata <- list(const=const, sat=sat, shallow=shallow)

test_that("deldood", {
          expect_equal(sum(nsat), ns)
          expect_equal(sat$deldood[1,], c(-1,  0, 0))
          expect_equal(sat$deldood[2,], c( 0, -1, 0))
          expect_equal(sat$deldood[3,], c(-2, -2, 0))
})

test_that("constituents were read correctly", {
        expect_equal(name[1], "Z0")
        expect_equal(name[2], "SA")
        expect_equal(kmpr[2], "SSA")
        i <- which(name == "M2")
        expect_equal(freq[i], 1/12.4206011981605)
})

test_that("shallow constitutents", {
          shallow <- data.frame(iconst=iconst, coef=coef, iname=iname)
          expect_equal(tidedata$shallow$iconst[1:5], c(26,26,27,27,30))
          expect_equal(tidedata$shallow$coef[1:5], c(2, -1, 1, -1, 2))
          expect_equal(tidedata$shallow$iname[1:5], c(19, 13, 57, 13, 48))
          expect_equal(df[48], 0.08051140070000)
          expect_equal(ishallow[143:146], c(242, 245, 246, 248))
          expect_equal(nshallow[143:146], c(3, 1, 2, 4))
})

test_that("doodson", {
          expect_equal(tidedata$const$doodsonspecies[c(2,3,10)], c(0,0,-1))
          expect_equal(tidedata$const$doodsonamp[c(2,3,10)], c(0.01160000000000,0.07299000000000,0.01153000000000))
})

test_that("ancillary code", {
    ## Test against matlab t_astron, with a randomly-picked calibration time
    t <- as.POSIXct("2008-01-22 18:50:24", tz="GMT")
    a <- tidemAstron(t)
    expect_equal(a$astro, c(1.28861316428, 0.33390620851, 0.83751937277, 0.14234854462, 0.08559663825, 0.78633079279), tolerance=1e-8)
    expect_equal(a$ader,  c(0.96613680803, 0.03660110127, 0.00273790931, 0.00030945407, 0.00014709388, 0.00000013082), tolerance=1e-8)
    vuf <- tidemVuf(t, 48)
    expect_equal(vuf$v, c(0.57722632857477), tolerance=1e-8)
    expect_equal(vuf$u, c(0), tolerance=1e-8)
    expect_equal(vuf$f, c(1), tolerance=1e-8)
    vuf <- tidemVuf(t, c(48, 49), 45)
    expect_equal(vuf$v, c(0.57722632857477,0.62841490855698), tolerance=1e-8)
    expect_equal(vuf$u, c(0.00295677805220,0.00180270946435), tolerance=1e-8)
    expect_equal(vuf$f, c(0.96893771510868,0.98142639461951), tolerance=1e-8)
})


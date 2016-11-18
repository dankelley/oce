rm(list=ls())
## This script requires a certain data file.
library(oce)
source("~/src/oce/R/cm.R")
file <- "/data/archive/sleiwex/2008/moorings/m11/cm/interocean_0811786/manufacturer/cm_interocean_0811786.s4a.tab"
filename <- file
sep <- "\t"
lines <- readLines(file, n=20)
headerEnd <- grep("Sample", lines)
if (length(headerEnd) == 0) stop("Cannot find header in first 20 lines; no lines contain 'Sample'")
if (length(headerEnd) > 1) stop("Cannot find header in first 20 lines; two lines contain 'Sample'")
## Find serial-number line (typically first line)
type <- "unknown"
serialNumber <- "unknown"
version <- "unknown"
experiment <- "unknown"
try({
    if (1 == length(grep("Serial No.", lines))) {
        i <- grep("Serial No.", lines)
        items <- strsplit(lines[i], "\t")[[1]]
        grab <- function(name, default)
            if (length(w<-which(items==name))) items[w+1] else default
        type <- grab("Type:", type)
        serialNumber <- grab("Serial No.", serialNumber)
        version <- grab("Version:", version)
        experiment <- grab("Header:", experiment)
    }
})
## serialNumber <- if (length(grep("Serial", lines)))
##     metadata$serialNumber <- items[i+1]
##     else if (length(grep("Version", items[i])))
##         metadata$version <- items[i+1]
##     else if (length(grep("Type", items[i])))
##         metadata$type <- items[i+1]
## }


## Get names of columns. I have NO idea whether these are fixed names.
names <- strsplit(lines[headerEnd], "\t")[[1]]
names <- gsub("^ *", "", gsub(" *$", "", names))
names
## Make names be more sensible
## Read data. Must use fill=TRUE because not all lines have the same number of data
d <- read.table(file, sep=sep, fill=TRUE, skip=headerEnd+1, header=FALSE, stringsAsFactors=FALSE)
##d <- read.csv("s4.csv", skip=headerEnd+1, header=FALSE, stringsAsFactors=FALSE)
dim <- dim(d)
## Chop last two lines, which contain statistical summaries
d <- d[seq.int(1, dim(d)[1]-2),]
names(d) <- names
## Need to construct times because the day string only occurs in first line and day starts
t0 <- as.POSIXct(paste(d[1,2], d[1,3]), format="%m/%d/%Y %H:%M:%S", tz="UTC")
t1 <- as.POSIXct(paste(d[1,2], d[2,3]), format="%m/%d/%Y %H:%M:%S", tz="UTC")
dt <- as.numeric(t1) - as.numeric(t0)
time <- seq(t0, by=dt, length.out=dim(d)[1])

## Do not bother trying to decode all of the columns. The sample dataset has
## several columns with duplicate names, several unnamed columns, and columns
## whose names mean very little to me. It also has extraneous stuff that
## doesn't belong in a data file (e.g. apparently, computed progressive-vector
## data). I am just going to grab what seems to be a minimal set of known
## quantities. I'm retaining the code above in case a user can decode other
## columns.
rval <- new("cm",
            time=time,
            u=d$Veast/100, v=d$Vnorth/100)
rval <- oceSetData(rval, name="salinity", value=d$Sal,
                   unit=list(unit=expression(), scale="PSS-78"),
                   originalName="Sal")
rval <- oceSetData(rval, name="temperature", value=d[["T-Temp"]],
                   unit=list(unit=expression(degree*C), scale="IPTS-68"),
                   originalName="T-Temp")
rval <- oceSetData(rval, name="pressure", value=swPressure(d$Depth, eos="unesco"),
                   unit=list(unit=expression(dbar), scale=""),
                   originalName="-")
rval <- oceSetMetadata(rval, "filename", filename)
rval <- oceSetMetadata(rval, "type", type)
rval <- oceSetMetadata(rval, "serialNumber", serialNumber)
rval <- oceSetMetadata(rval, "version", version)
summary(rval)
plot(rval)


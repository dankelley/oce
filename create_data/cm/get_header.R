file <- "cm_interocean_0811786.s4a.tab"
start <- readLines(file, n=10)
dnames <- strsplit(start[4], '\t')[[1]]
a <- dnames
dnames <- gsub(" *$", "", gsub("^ *", "", dnames))
dnames <- make.names(dnames)
dnames[dnames=="Sample.."] <- "sample"
dnames[dnames=="Date"] <- "date"
dnames[dnames=="Time"] <- "time"
dnames[dnames=="decS"] <- "decS"
dnames[dnames=="Vnorth"] <- "v"
dnames[dnames=="Veast"] <- "u"
#dnames[dnames=="Speed"] <- "speed"
dnames[dnames=="Dir"] <- "direction"
#dnames[dnames=="Vref"] <- "Vref"
#dnames[dnames=="Hx"] <- "hx"
#dnames[dnames=="Hy"] <- "hy"
#dnames[dnames=="Cond"] <- "conductivity"
#dnames[dnames=="T.Temp"] <- "temperature"
dnames[dnames=="Depth"] <- "depth"
#dnames[dnames=="Hdg"] <- "heading"
dnames[dnames=="Sal"] <- "salinity"
#dnames[dnames=="Dens"] <- "density"
#dnames[dnames=="SV"] <- "soundVelocity"
#dnames[dnames=="N.S.Dist"] <- "NSDist"
#dnames[dnames=="E.W.Dist"] <- "EWDist"
#dnames[dnames=="SRB.Date"] <- "SRBDate"
#dnames[dnames=="SRB.Time"] <- "SRBTime"
#dnames[dnames=="Vref.1"] <- "Vref2"
#dnames[dnames=="Hx.1"] <- "Hx2"
#dnames[dnames=="Hy.1"] <- "Hy2"
#dnames[dnames=="Cond.1"] <- "conductivity2"
#dnames[dnames=="T-Temp.1"] <- "temperature2"
#dnames[dnames=="Depth.1"] <- "depth2"
#dnames[dnames=="Sal.1"] <- "salinity2"
#dnames[dnames=="Dens.1"] <- "density2"
#dnames[dnames=="SV.1"] <- "soundVelocity2"
data <- read.delim(file, skip=5, sep="\t", col.names=dnames, stringsAsFactors=FALSE)
print(head(data))

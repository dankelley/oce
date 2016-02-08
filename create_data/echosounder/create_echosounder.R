message("THIS IS NOT CORRECT. It seems that I have smoothed the existing dataset and not just decimated")
library(oce)
## I would like to make dataset have dim(ECHOSOUNDER@data$a) = c(736,88) but doing that with decimate()
## does not yield the smoothness in the existing datafile. The present code is useless except perhaps
## to remind me where the original file resided, what times I used for chopping, etc.

#echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_161403.dt4")
echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4")
#echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_164323.dt4")
#echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4")
#echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_164742.dt4")
#echosounder <- read.oce("/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_165611.dt4")

echosounder <- subset(echosounder, depth < 40)
#echosounder <- subset(echosounder, time > as.POSIXct("2008-07-01 16:44:30", tz="UTC"))
echosounder <- decimate(echosounder, c(2, 40))
print(dim(echosounder@data$a))
print("Hope for above dim 736 88 or so")
plot(echosounder, drawBottom=TRUE, despike=TRUE, which="zx image")
#plot(echosounder2, drawBottom=TRUE)
save(echosounder, file="echosounder.rda")
tools::resaveRdaFiles("echosounder.rda")


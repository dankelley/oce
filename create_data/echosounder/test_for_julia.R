library(oce)
file <- "/Users/kelley/Dropbox/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4"

# Timing test: the next takes 0.14 s (on a 10MiB file)
#    system.time(echosounder <- read.oce(file))

# Next is to record the output, for comparison during the coding of the
# OceanAnalysis Julia library.
e <- read.oce(file, debug=5)
summary(e)
# imagep(log10(echosounder[["a"]]))
print(head(e[["time"]]))


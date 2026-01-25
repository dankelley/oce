library(oce)
file <- "/Users/kelley/Dropbox/data/archive/sleiwex/2008/fielddata/2008-07-01/Merlu/Biosonics/20080701_163942.dt4"
# Next is for comparison during OceanAnalysis development
e <- read.oce(file, debug = 1)
t <- e[["time"]]
a <- e[["a"]]
N <- 10
cat("File starts at ", format(t[1]), ", with following decoded 'a' values, followed by ", dim(a)[2] - N, " other values\n", sep="")
print(a[1,1:N])
cat(vectorShow(t, n=20))
png("test_for_julia.png")
plot(e)


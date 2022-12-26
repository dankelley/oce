library(oce)
data(ctd)
n <- 4
ctds <- vector("list", n)
for (i in 1:n) {
    tmp <- ctd
    tmp@metadata$startTime <- tmp@metadata$startTime + i * 3600
    ctds[[i]] <- tmp
}
s <- as.section(ctds)
s[["time"]]
plot(s, which="temperature", xtype="time")

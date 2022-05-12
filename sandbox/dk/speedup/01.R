f <- "~/git/oce.Rcheck/oce-Ex.timings"
d <- read.delim(f, sep="\t", skip=1,
    col.names=c("name","user","system","elapsed", "junk"))
d$junk <- NULL # all NA, anyway
slow <- d$elapsed >= 1.0
dslow <- d[slow,]
oslow <- order(dslow$elapsed, decreasing=TRUE)
dslow <- dslow[oslow,]
cat("Total elapsed time: ", sum(d$elapsed), "\n")
print(d[order(d$elapsed, decreasing=TRUE),])


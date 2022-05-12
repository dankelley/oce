f <- "~/git/oce.Rcheck/oce-Ex.timings"
d <- read.delim(f, sep="\t", skip=1,
    col.names=c("name","user","system","elapsed", "junk"))
d$junk <- NULL # all NA, anyway
slow <- d$elapsed >= 1.0
dslow <- d[slow,]
oslow <- order(dslow$elapsed, decreasing=TRUE)
dslow <- dslow[oslow,]
cat("The", length(oslow), "cases taking > 1s sum to", sum(dslow[oslow,"elapsed"]), "sec\n")
cat("Below is a summary of all timings\n")
print(d[order(d$elapsed, decreasing=TRUE),])


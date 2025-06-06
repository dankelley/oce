library(oce)
options(width=100)                     # makes summaries easier to read
file <- "~/Dropbox/oce_secret_data/01.rsk"
if (file.exists(file)) {
    d <- read.oce(file)
    summary(d)
    if (!interactive()) png("rsk01a.png")
    plot(d)
    if (!interactive()) dev.off()
    if (!interactive()) png("rsk01b.png")
    par(mfrow=c(2, 1))
    hist(d[["cond12"]])
    hist(d[["conductivity"]])
    if (!interactive()) dev.off()
}


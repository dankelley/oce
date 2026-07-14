library(oce)
options(width = 100) # makes summaries easier to read
files <- c("01", "01b")[1]
for (file in files) {
    filename <- paste0("~/Dropbox/oce_secret_data/", file, ".rsk")
    message(filename)
    if (file.exists(filename)) {
        pngname <- paste0("rsk_", file, "_%02d.png")
        message(pngname)
        if (!interactive()) png(pngname)
        d <- read.oce(filename)
        summary(d)
        plot(d, simplify = NA)
        par(mfrow = c(2, 1))
        hist(d[["cond12"]])
        hist(d[["conductivity"]])
        if (!interactive()) dev.off()
    }
}

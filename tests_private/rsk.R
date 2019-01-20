library(oce)
options(width=100)                     # makes summaries easier to read
options(warn=2)                        # die on warning, to catch unrecognized SBE names
file <- "~/Dropbox/oce_secret_data/01.rsk"
if (file.exists(file)) {
    testthat::expect_warning(d <- read.oce(file), "old Ruskin file detected")
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


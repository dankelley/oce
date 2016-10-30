library(oce)

path <- "~/Dropbox/oce-working-notes/tests/adp-rdi"
files <- list.files(path, "*", recursive=TRUE, full.names=TRUE)

n <- 0
for (file in files) {
    try({
        cat("\nfile '", file, "' ", sep="")
        if ("adp/rdi" == oceMagic(file)) {
            cat("is adp/rdi\n")
            d <- read.oce(file)
            n <- n + 1
        } else {
            cat("is not adp/rdi, so it is being skipped\n")
        }
    })
}
cat("Successfully checked", n, "ADP files in", path, "\n")


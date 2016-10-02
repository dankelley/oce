library(oce)

files <- list.files("~/Dropbox/oce-working-notes/tests/adp-rdi", "*", recursive=TRUE, full.names=TRUE)

for (file in files) {
    try({
        cat("\nfile '", file, "' ", sep="")
        if ("adp/rdi" == oceMagic(file)) {
            cat("is adp/rdi\n")
            d <- read.oce(file)
        } else {
            cat("is not adp/rdi, so it is being skipped\n")
        }
    })
}

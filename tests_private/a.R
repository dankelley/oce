library(oce)

files <- list.files("~/Dropbox/oce-working-notes/tests/adp-rdi", "*", recursive=TRUE, full.names=TRUE)
files <- '/Users/kelley/Dropbox/oce-working-notes/tests/adp-rdi/moored_workhorse_600kHz/adp_rdi_5249.000' # mismatch at ens 29555
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

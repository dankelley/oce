library(oce)

## message("next file has an unusual error -- fix before proceeding with tests")
## d<-read.adp.rdi("~/Dropbox/oce-working-notes/tests/adp-rdi/vmdas_workhorse_300kHz_02/ADCP066_000000.STA")
## d<-read.adp.rdi("~/Dropbox/oce-working-notes/tests/adp-rdi/winriver_vesselmounted_1200kHz/DATA_20080701123035_000r.000")
## stop()

files <- list.files("~/Dropbox/oce-working-notes/tests/adp-rdi", "*", recursive=TRUE, full.names=TRUE)
files <- '/Users/kelley/Dropbox/oce-working-notes/tests/adp-rdi/vmdas_workhorse_300kHz_01/ADCP061_000000.NMS' # multibyte string error
for (file in files) {
    try({
        cat("file '", file, "' ", sep="")
        if ("adp/rdi" == oceMagic(file)) {
            cat(" is adp/rdi\n")
            d <- read.oce(file)
        } else {
            cat(" is not adp/rdi, so it is being skipped\n")
        }
    })
}

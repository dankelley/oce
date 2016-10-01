library(oce)

## message("next file has an unusual error -- fix before proceeding with tests")
## d<-read.adp.rdi("~/Dropbox/oce-working-notes/tests/adp-rdi/vmdas_workhorse_300kHz_02/ADCP066_000000.STA")
## d<-read.adp.rdi("~/Dropbox/oce-working-notes/tests/adp-rdi/winriver_vesselmounted_1200kHz/DATA_20080701123035_000r.000")
## stop()

files <- list.files("~/Dropbox/oce-working-notes/tests/adp-rdi", "*", recursive=TRUE, full.names=TRUE)
#files <- '~/Dropbox/oce-working-notes/tests/adp-rdi/moored_workhorse_1200kHz/adp_rdi_1997.000'
for (file in files) {
    try({
        if ("adp/rdi" == oceMagic(file)) {
            cat("file '", file, "'\n", sep="")
            d <- read.oce(file)
        }
    })
}

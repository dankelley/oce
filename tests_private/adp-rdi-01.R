library(oce)

files <- list.files("~/Dropbox/oce-working-notes/tests/adp-rdi", "*", recursive=TRUE, full.names=TRUE)
for (file in files) {
    try({
        if ("adp/rdi" == oceMagic(file)) {
            cat("file '", file, "'\n", sep="")
            d <- read.oce(file)
        }
    })
}

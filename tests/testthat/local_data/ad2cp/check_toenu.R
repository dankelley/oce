library(oce)
files <- list.files(".", pattern="*ad2cp$")
for (file in files) {
    message(file)
    d <- read.oce(file)
    denu <- toEnu(d)
}

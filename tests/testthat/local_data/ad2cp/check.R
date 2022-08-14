library(oce)
files <- list.files(".", pattern="*ad2cp$")
for (file in files) {
    message(file)
    print(read.oce(file, which="?"))
}

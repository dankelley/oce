library(oce)

path <- "~/Dropbox/oce-working-notes/tests/adp-rdi"
files <- list.files(path, "*", recursive=TRUE, full.names=TRUE)
options(warn=1)                        # see warnings when they occur

n <- 0
if (!interactive()) png("adp_rdi_01-%02d.png")
for (file in files) {
    try({
        if ("adp/rdi" == oceMagic(file)) {
            cat(file, " is adp/rdi\n", sep="")
            d <- read.oce(file)
            summary(d)
            plot(d)
            n <- n + 1
        } else {
            if (!grepl(".*R$", file) && !grepl(".*txt$", file) && !grepl(".*out$", file) && !grepl(".*~$", file)) {
                cat(file, " is not adp/rdi, so it is being skipped\n", sep="")
            }
        }
    })
}
if (!interactive()) dev.off()
cat("Successfully checked ", n, " ADP files in ", path, "\n", sep="")
